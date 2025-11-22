module restart_dynamics

! Write and read dynamics fields from the restart file.  For exact restart
! it is necessary to write all element data, including duplicate columns,
! to the file.  The namelist option, se_write_restart_unstruct, is
! available to write just the unique columns to the restart file using the
! same unstructured grid used by the history and initial files.  This
! results in the introduction of a roundoff size difference on restart, but
! writes the fields in the unstructured grid format which is easier to
! modify if the user desires to introduce perturbations or other
! adjustments into the run.  The restart file containing the unstructured
! grid format may also be used for an initial run.

use shr_kind_mod,     only: r8 => shr_kind_r8
use spmd_utils,       only: iam, masterproc

use dyn_grid,         only: timelevel, fvm, elem, edgebuf
use dyn_comp,         only: dyn_import_t, dyn_export_t, dyn_init, write_restart_unstruct
use hycoef,           only: init_restart_hycoef, write_restart_hycoef, &
                            hyai, hybi, ps0
use ref_pres,         only: ptop_ref

use pio,              only: pio_global, pio_unlimited, pio_offset_kind, pio_int, pio_double, &
                            pio_seterrorhandling, pio_bcast_error, pio_noerr, &
                            file_desc_t, var_desc_t, io_desc_t, &
                            pio_inq_dimid, pio_inq_dimlen, pio_inq_varid, &
                            pio_def_dim, pio_def_var,  &
                            pio_enddef, &
                            pio_initdecomp, pio_freedecomp, pio_setframe, &
                            pio_put_att, pio_put_var, pio_write_darray, &
                            pio_get_att, pio_get_var, pio_read_darray

use cam_pio_utils,    only: pio_subsystem, cam_pio_handle_error
use cam_grid_support, only: cam_grid_header_info_t, &
                            cam_grid_write_var, cam_grid_get_decomp, cam_grid_dimensions

use parallel_mod,     only: par
use thread_mod,       only: horz_num_threads
use dof_mod,          only: UniquePoints
use element_mod,      only: element_t

use edge_mod,         only: initEdgeBuffer, edgeVpack, edgeVunpack, FreeEdgeBuffer
use edgetype_mod,     only: EdgeBuffer_t
use bndry_mod,        only: bndry_exchange

use fvm_control_volume_mod, only: fvm_struct

implicit none
private
save

public :: init_restart_dynamics
!public :: write_restart_dynamics
!public :: read_restart_dynamics

! these variables are module data so they can be shared between the
! file definition and write phases
type(var_desc_t)              :: psdry_desc, udesc, vdesc, tdesc
type(var_desc_t), allocatable :: qdesc_dp(:)
type(var_desc_t)              :: dp_fvm_desc
type(var_desc_t), pointer     :: c_fvm_desc(:)

integer, private :: nelem_tot = -1 ! Correct total number of elements

!=========================================================================================
CONTAINS
!=========================================================================================

subroutine init_nelem_tot()
  use spmd_utils,     only: mpicom
  use mpi,            only: mpi_integer, mpi_sum
  use dimensions_mod, only: nelemd

  integer :: ierr

  if (nelem_tot < 0) then
    call MPI_Allreduce(nelemd, nelem_tot, 1, MPI_INTEGER, MPI_SUM, mpicom, ierr)
  end if
end subroutine init_nelem_tot

subroutine init_restart_dynamics(file, dyn_out)
   use cam_ccpp_cap,              only: cam_model_const_properties
   use ccpp_constituent_prop_mod, only: ccpp_constituent_prop_ptr_t
   use pio, only: file_desc_t
   use dyn_comp, only: dyn_export_t
   use cam_grid_support, only: cam_grid_header_info_t, cam_grid_id
   use cam_grid_support, only: cam_grid_write_attr
   use dimensions_mod,   only: np, npsq, ne, nelemd, ntrac, fv_nphys
   ! Define dimensions, variables, attributes for restart file.

   ! This is not really an "init" routine.  It is called before
   ! write_restart_dynamics every time an restart is written.

   ! arguments
   type(file_desc_t),  intent(inout) :: file
   type(dyn_export_t), intent(in)    :: dyn_out

   ! local variables
   integer :: i
   integer :: vdimids(2)
   integer :: nlev_dimid
   integer :: ncol_dimid
   integer :: ncol_fvm_dimid
   integer :: time_dimid

   integer :: ierr, err_handling

   integer :: grid_id
   type(cam_grid_header_info_t) :: info
   integer :: constituent_idx
   type(ccpp_constituent_prop_ptr_t), pointer :: const_props(:)
   character(len=256) :: const_diag_name

   !----------------------------------------------------------------------------

   call init_nelem_tot()
   call init_restart_hycoef(file, vdimids)
   nlev_dimid = vdimids(1)

   call pio_seterrorhandling(File, pio_bcast_error, err_handling)

   ierr = PIO_Def_Dim(File, 'time', PIO_UNLIMITED, time_dimid)

   ! GLL restart fields

   ! number of columns written to restart depends on whether all columns in the
   ! element structures are written, or just the unique columns (unstructured grid)
   if (write_restart_unstruct) then
      grid_id = cam_grid_id('GLL')
      call cam_grid_write_attr(File, grid_id, info)
      ncol_dimid = info%get_hdimid(1)
   else
      ierr = PIO_Def_Dim(File,'nenpnp', nelem_tot*np*np, ncol_dimid)
      ierr = PIO_Put_Att(File, PIO_GLOBAL, 'ne', ne)
      ierr = PIO_Put_Att(File, PIO_GLOBAL, 'np', np)
   end if

   ierr = PIO_Def_Var(File, 'PSDRY', pio_double, (/ncol_dimid, time_dimid/), psdry_desc)
   ierr = PIO_Def_Var(File, 'U', pio_double, (/ncol_dimid, nlev_dimid, time_dimid/), Udesc)
   ierr = PIO_Def_Var(File, 'V', pio_double, (/ncol_dimid, nlev_dimid, time_dimid/), Vdesc)
   ierr = PIO_Def_Var(File, 'T', pio_double, (/ncol_dimid, nlev_dimid, time_dimid/), Tdesc)

   const_props => cam_model_const_properties()
   allocate(qdesc_dp(size(const_props)))
   do constituent_idx = 1, size(const_props)
      ! Grab constituent diagnostic name:
      call const_props(constituent_idx)%diagnostic_name(const_diag_name)
      ierr = PIO_Def_Var(File,"dp"//trim(const_diag_name), pio_double, &
                         (/ncol_dimid, nlev_dimid, time_dimid/), Qdesc_dp(constituent_idx))
   end do

   ! CSLAM restart fields

   if (fv_nphys > 0) then

      grid_id = cam_grid_id('FVM')
      call cam_grid_write_attr(File, grid_id, info)
      ncol_fvm_dimid = info%get_hdimid(1)

      ierr = PIO_Def_Var(File, 'dp_fvm', pio_double, &
         (/ncol_fvm_dimid, nlev_dimid, time_dimid/), dp_fvm_desc)

      allocate(c_fvm_desc(size(const_props)))
      do constituent_idx = 1, size(const_props)
         call const_props(constituent_idx)%diagnostic_name(const_diag_name)
         ierr = PIO_Def_Var(File, trim(const_diag_name)//"_fvm", pio_double, &
            (/ncol_fvm_dimid, nlev_dimid, time_dimid/), c_fvm_desc(constituent_idx))
      end do

   end if

   call pio_seterrorhandling(File, err_handling)

end subroutine init_restart_dynamics

!=========================================================================================
! Private
!=========================================================================================

function get_restart_decomp(elem, lev) result(ldof)
   use dimensions_mod, only: nelemd, np

   ! Get the integer mapping of a variable in the dynamics decomp in memory.
   ! The canonical ordering is as on the file. A 0 value indicates that the
   ! variable is not on the file (eg halo or boundary values)

   type(element_t), intent(in) :: elem(:)
   integer,         intent(in) :: lev
   integer,         pointer    :: ldof(:)

   integer :: i, j, k, ie
   !----------------------------------------------------------------------------

   allocate(ldof(nelemd*np*np*lev))

   j = 1
   do k = 1, lev
      do ie = 1, nelemd
         do i = 1, np*np
            ldof(j) = (elem(ie)%GlobalID-1)*np*np + (k-1)*nelem_tot*np*np + i
            j = j + 1
         end do
      end do
   end do

end function get_restart_decomp

!=========================================================================================

function get_restart_decomp_fvm(elem, lev) result(ldof)
   use dimensions_mod, only: nc, nelemd

   type(element_t), intent(in) :: elem(:)
   integer,         intent(in) :: lev
   integer,         pointer    :: ldof(:)

   integer :: i, j, k, ie
   !----------------------------------------------------------------------------

   allocate(ldof(nelemd*nc*nc*lev))

   j = 1
   do k = 1, lev
      do ie = 1, nelemd
         do i = 1, nc*nc
            ldof(j) = (elem(ie)%GlobalID-1)*nc*nc + (k-1)*nelem_tot*nc*nc + i
            j = j + 1
         end do
      end do
   end do

end function get_restart_decomp_fvm

!=========================================================================================

end module restart_dynamics
