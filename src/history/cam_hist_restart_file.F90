module cam_hist_restart_file
   use pio,                 only: var_desc_t
   use cam_history_support, only: max_fieldname_len
   use shr_kind_mod,        only: r4 => shr_kind_r4
   use shr_kind_mod,        only: r8 => shr_kind_r8

   implicit none
   private

   integer, parameter :: max_dimensions = 4

   type restart_variable_t
      type(var_desc_t), pointer        :: vdesc => null()
      integer                          :: var_type
      integer                          :: number_of_dimensions
      integer                          :: dimension_ids(max_dimensions)
      character(len=max_fieldname_len) :: var_name
      logical                          :: fill_set = .false.
      integer                          :: integer_fill
      real(r4)                         :: real_fill
      real(r8)                         :: double_fill
    end type restart_variable_t

    type restart_dimension_t
      integer                          :: dimension_length
      integer                          :: dimension_id
      character(len=max_fieldname_len) :: dimension_name
    end type restart_dimension_t

    !
    !   The size of these parameters should match the assignments in restart_vars_setnames and restart_dims_setnames below
    !
    integer, parameter :: num_restart_vars = 24
    integer, parameter :: num_restart_dims =  9
    type(restart_variable_t)  :: restart_vars(num_restart_vars)
    type(restart_dimension_t) :: restart_dims(num_restart_dims)

    integer, parameter :: max_num_fields = 1000
    integer, parameter :: max_var_mdims  = 1

    integer, parameter :: num_configs_dim_ind        =  1
    integer, parameter :: max_string_len_dim_ind     =  2
    integer, parameter :: max_fieldname_len_dim_ind  =  3
    integer, parameter :: max_num_fields_dim_ind     =  4
    integer, parameter :: max_chars_dim_ind          =  5
    integer, parameter :: maxvarmdims_dim_ind        =  6
    integer, parameter :: registeredmdims_dim_ind    =  7
    integer, parameter :: max_hcoordname_len_dim_ind =  8
    integer, parameter :: max_num_split_files_dim_ind    =  9

    public :: set_restart_variable_names
    public :: set_restart_dimension_names

CONTAINS

   subroutine set_restart_variable_names()
      use pio, only: pio_int, pio_double, pio_char
      integer :: rvar_index

      rvar_index = 1
      restart_vars(rvar_index)%var_name = 'has_rh_file'
      restart_vars(rvar_index)%var_type = pio_int
      restart_vars(rvar_index)%number_of_dimensions = 1
      restart_vars(rvar_index)%dimension_ids(1) = num_configs_dim_ind

      rvar_index = rvar_index + 1
      restart_vars(rvar_index)%var_name = 'output_frequency'
      restart_vars(rvar_index)%var_type = pio_char
      restart_vars(rvar_index)%number_of_dimensions = 2
      restart_vars(rvar_index)%dimension_ids(1) = max_chars_dim_ind
      restart_vars(rvar_index)%dimension_ids(2) = num_configs_dim_ind

      rvar_index = rvar_index + 1
      restart_vars(rvar_index)%var_name = 'number_of_fields'
      restart_vars(rvar_index)%var_type = pio_int
      restart_vars(rvar_index)%number_of_dimensions = 1
      restart_vars(rvar_index)%dimension_ids(1) = num_configs_dim_ind

      rvar_index = rvar_index + 1
      restart_vars(rvar_index)%var_name = 'num_satellite_columns'
      restart_vars(rvar_index)%var_type = pio_int
      restart_vars(rvar_index)%number_of_dimensions = 1
      restart_vars(rvar_index)%dimension_ids(1) = num_configs_dim_ind

      rvar_index = rvar_index + 1
      restart_vars(rvar_index)%var_name = 'max_samples'
      restart_vars(rvar_index)%var_type = pio_int
      restart_vars(rvar_index)%number_of_dimensions = 1
      restart_vars(rvar_index)%dimension_ids(1) = num_configs_dim_ind

      rvar_index = rvar_index + 1
      restart_vars(rvar_index)%var_name = 'current_file_path'
      restart_vars(rvar_index)%var_type = pio_char
      restart_vars(rvar_index)%number_of_dimensions = 3
      restart_vars(rvar_index)%dimension_ids(1) = max_string_len_dim_ind
      restart_vars(rvar_index)%dimension_ids(2) = num_configs_dim_ind
      restart_vars(rvar_index)%dimension_ids(3) = max_num_split_files_dim_ind

      rvar_index = rvar_index + 1
      restart_vars(rvar_index)%var_name = 'current_file_name'
      restart_vars(rvar_index)%var_type = pio_char
      restart_vars(rvar_index)%number_of_dimensions = 3
      restart_vars(rvar_index)%dimension_ids(1) = max_string_len_dim_ind
      restart_vars(rvar_index)%dimension_ids(2) = num_configs_dim_ind
      restart_vars(rvar_index)%dimension_ids(3) = max_num_split_files_dim_ind

      rvar_index = rvar_index + 1
      restart_vars(rvar_index)%var_name = 'precision'
      restart_vars(rvar_index)%var_type = pio_char
      restart_vars(rvar_index)%number_of_dimensions = 2
      restart_vars(rvar_index)%dimension_ids(1) = max_chars_dim_ind
      restart_vars(rvar_index)%dimension_ids(2) = num_configs_dim_ind

      rvar_index = rvar_index + 1
      restart_vars(rvar_index)%var_name = 'interval_start_time'
      restart_vars(rvar_index)%var_type = pio_double
      restart_vars(rvar_index)%number_of_dimensions = 1
      restart_vars(rvar_index)%dimension_ids(1) = num_configs_dim_ind

      rvar_index = rvar_index + 1
      restart_vars(rvar_index)%var_name = 'field_name'
      restart_vars(rvar_index)%var_type = pio_char
      restart_vars(rvar_index)%number_of_dimensions = 3
      restart_vars(rvar_index)%dimension_ids(1) = max_fieldname_len_dim_ind
      restart_vars(rvar_index)%dimension_ids(2) = max_num_fields_dim_ind
      restart_vars(rvar_index)%dimension_ids(3) = num_configs_dim_ind

      rvar_index = rvar_index + 1
      restart_vars(rvar_index)%var_name = 'field_avg_flag'
      restart_vars(rvar_index)%var_type = pio_char
      restart_vars(rvar_index)%number_of_dimensions = 3
      restart_vars(rvar_index)%dimension_ids(1) = max_chars_dim_ind
      restart_vars(rvar_index)%dimension_ids(2) = max_num_fields_dim_ind
      restart_vars(rvar_index)%dimension_ids(3) = num_configs_dim_ind

      rvar_index = rvar_index + 1
      restart_vars(rvar_index)%var_name = 'field_decomposition_type'
      restart_vars(rvar_index)%var_type = pio_int
      restart_vars(rvar_index)%number_of_dimensions = 2
      restart_vars(rvar_index)%dimension_ids(1) = max_num_fields_dim_ind
      restart_vars(rvar_index)%dimension_ids(2) = num_configs_dim_ind
      restart_vars(rvar_index)%fill_set = .true.
      restart_vars(rvar_index)%integer_fill = 0

      rvar_index = rvar_index + 1
      restart_vars(rvar_index)%var_name = 'field_num_vertical_levels'
      restart_vars(rvar_index)%var_type = pio_int
      restart_vars(rvar_index)%number_of_dimensions = 2
      restart_vars(rvar_index)%dimension_ids(1) = max_num_fields_dim_ind
      restart_vars(rvar_index)%dimension_ids(2) = num_configs_dim_ind
      restart_vars(rvar_index)%fill_set = .true.
      restart_vars(rvar_index)%integer_fill = 0

      rvar_index = rvar_index + 1
      restart_vars(rvar_index)%var_name = 'history_restart_path'
      restart_vars(rvar_index)%var_type = pio_char
      restart_vars(rvar_index)%number_of_dimensions = 2
      restart_vars(rvar_index)%dimension_ids(1) = max_string_len_dim_ind
      restart_vars(rvar_index)%dimension_ids(2) = num_configs_dim_ind

      rvar_index = rvar_index + 1
      restart_vars(rvar_index)%var_name = 'field_cell_methods'
      restart_vars(rvar_index)%var_type = pio_char
      restart_vars(rvar_index)%number_of_dimensions = 3
      restart_vars(rvar_index)%dimension_ids(1) = max_chars_dim_ind
      restart_vars(rvar_index)%dimension_ids(2) = max_num_fields_dim_ind
      restart_vars(rvar_index)%dimension_ids(3) = num_configs_dim_ind

      rvar_index = rvar_index + 1
      restart_vars(rvar_index)%var_name = 'field_long_name'
      restart_vars(rvar_index)%var_type = pio_char
      restart_vars(rvar_index)%number_of_dimensions = 3
      restart_vars(rvar_index)%dimension_ids(1) = max_chars_dim_ind
      restart_vars(rvar_index)%dimension_ids(2) = max_num_fields_dim_ind
      restart_vars(rvar_index)%dimension_ids(3) = num_configs_dim_ind

      rvar_index = rvar_index + 1
      restart_vars(rvar_index)%var_name = 'field_units'
      restart_vars(rvar_index)%var_type = pio_char
      restart_vars(rvar_index)%number_of_dimensions = 3
      restart_vars(rvar_index)%dimension_ids(1) = max_chars_dim_ind
      restart_vars(rvar_index)%dimension_ids(2) = max_num_fields_dim_ind
      restart_vars(rvar_index)%dimension_ids(3) = num_configs_dim_ind

      rvar_index = rvar_index + 1
      restart_vars(rvar_index)%var_name = 'field_fill_flag'
      restart_vars(rvar_index)%var_type = pio_int
      restart_vars(rvar_index)%number_of_dimensions = 2
      restart_vars(rvar_index)%dimension_ids(1) = max_num_fields_dim_ind
      restart_vars(rvar_index)%dimension_ids(2) = num_configs_dim_ind

      rvar_index = rvar_index + 1
      restart_vars(rvar_index)%var_name = 'field_fill_value'
      restart_vars(rvar_index)%var_type = pio_double
      restart_vars(rvar_index)%number_of_dimensions = 2
      restart_vars(rvar_index)%dimension_ids(1) = max_num_fields_dim_ind
      restart_vars(rvar_index)%dimension_ids(2) = num_configs_dim_ind
      restart_vars(rvar_index)%fill_set = .true.
      restart_vars(rvar_index)%double_fill = 0.0_r8

      rvar_index = rvar_index + 1
      restart_vars(rvar_index)%var_name = 'field_mdims'
      restart_vars(rvar_index)%var_type = pio_int
      restart_vars(rvar_index)%number_of_dimensions = 3
      restart_vars(rvar_index)%dimension_ids(1) = maxvarmdims_dim_ind
      restart_vars(rvar_index)%dimension_ids(2) = max_num_fields_dim_ind
      restart_vars(rvar_index)%dimension_ids(3) = num_configs_dim_ind

      rvar_index = rvar_index + 1
      restart_vars(rvar_index)%var_name = 'mdim_names'
      restart_vars(rvar_index)%var_type = pio_char
      restart_vars(rvar_index)%number_of_dimensions = 2
      restart_vars(rvar_index)%dimension_ids(1) = max_hcoordname_len_dim_ind
      restart_vars(rvar_index)%dimension_ids(2) = registeredmdims_dim_ind

   end subroutine set_restart_variable_names

   subroutine set_restart_dimension_names(num_hist_configs, max_fields)
      use cam_history_support, only: max_string_len, max_chars, max_fieldname_len, registeredmdims
      use cam_grid_support,    only: max_hcoordname_len, max_split_files
      integer, intent(in) :: num_hist_configs
      integer, intent(in) :: max_fields

      restart_dims(num_configs_dim_ind)%dimension_name = 'number_of_hist_configs'
      restart_dims(num_configs_dim_ind)%dimension_length = num_hist_configs

      restart_dims(max_string_len_dim_ind)%dimension_name = 'max_string_length'
      restart_dims(max_string_len_dim_ind)%dimension_length = max_string_len

      restart_dims(max_fieldname_len_dim_ind)%dimension_name = 'max_fieldname_length'
      restart_dims(max_fieldname_len_dim_ind)%dimension_length = max_fieldname_len

      restart_dims(max_num_fields_dim_ind)%dimension_name = 'max_fields_per_configuration'
      restart_dims(max_num_fields_dim_ind)%dimension_length = max_fields

      restart_dims(max_chars_dim_ind)%dimension_name = 'max_chars'
      restart_dims(max_chars_dim_ind)%dimension_length = max_chars

      restart_dims(maxvarmdims_dim_ind)%dimension_name = 'max_variable_mdims'
      restart_dims(maxvarmdims_dim_ind)%dimension_length = max_var_mdims

      restart_dims(registeredmdims_dim_ind)%dimension_name = 'registered_mdims'
      restart_dims(registeredmdims_dim_ind)%dimension_length = registeredmdims

      restart_dims(max_hcoordname_len_dim_ind)%dimension_name = 'max_hcoordname_len'
      restart_dims(max_hcoordname_len_dim_ind)%dimension_length = max_hcoordname_len

      restart_dims(max_num_split_files_dim_ind)%dimension_name = 'max_num_split_files'
      restart_dims(max_num_split_files_dim_ind)%dimension_length = max_split_files

   end subroutine set_restart_dimension_names

end module cam_hist_restart_file
