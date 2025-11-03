"""
Use variable meta-data from "generate_registry_data.py"
to generate a CAM fortran file to manage physics restart 
information and writing (restart_physics.F90).
"""

# Python library import statements:
from collections import OrderedDict
import os.path

# CCPP Framework import statements
from ccpp_state_machine import CCPP_STATE_MACH
from fortran_tools import FortranWriter
# Exclude these standard names from init processing
# Some are internal names (e.g., suite_name)
# Some are from the CCPP framework (e.g., ccpp_num_constituents)
# Some are for efficiency and to avoid dependency loops (e.g., log_output_unit)
_EXCLUDED_STDNAMES = {'suite_name', 'suite_part',
                          'number_of_ccpp_constituents',
                          'number_of_ccpp_advected_constituents',
                          'ccpp_constituents',
                          'ccpp_constituent_tendencies',
                          'ccpp_constituent_properties',
                          'ccpp_constituent_minimum_values',
                          'ccpp_error_message',
                          'ccpp_error_code',
                          'log_output_unit', 'do_log_output',
                          'mpi_communicator', 'mpi_root', 'mpi_rank',
                          'number_of_mpi_tasks'}
# Variable input types
_INPUT_TYPES = set(['in', 'inout'])
_OUTPUT_TYPES = set(['out', 'inout'])

# Increase allowed line lengths needed to fit extra-long CCPP standard names:
_LINE_FILL_LEN = 150
_MAX_LINE_LEN = 200

##############
#Main function
##############

def write_restart_physics(cap_database, ic_names, registry_constituents,
                     restart_vars, outdir, file_find_func, source_paths, indent, logger,
                     phys_restart_filename=None):

    """
    Create restart_physics.F90 using a database created
       by the CCPP Framework generator (capgen).
    """

    #Initialize return message:
    retmsg = ""

    # Gather all the host model variables that are required by
    #    any of the compiled CCPP physics suites.
    in_vars, out_vars, constituent_set, retmsg = gather_ccpp_req_vars(cap_database, registry_constituents)

    # Quit now if there are missing variables
    if retmsg:
        return retmsg
    # end if

    # -----------------------------------------
    # Generate "restart_physics.F90" file:
    # -----------------------------------------

    # Open new file:
    if phys_restart_filename:
        ofilename = os.path.join(outdir, phys_restart_filename)
        # Get file name, ignoring file type:
        phys_restart_fname_str = os.path.splitext(phys_restart_filename)[0]
    else:
        ofilename = os.path.join(outdir, "restart_physics.F90")
        phys_restart_fname_str = "restart_physics"
    # end if

    # Log file creation:
    logger.info(f"Writing restart physics source file, {ofilename}")

    # Open file using CCPP's FortranWriter:
    file_desc = "Physics restart source file"
    with FortranWriter(ofilename, "w", file_desc,
                       phys_restart_fname_str,
                       line_fill=_LINE_FILL_LEN,
                       line_max=_MAX_LINE_LEN,
                       indent=indent) as outfile:

        # Add boilerplate code:
        outfile.write_preamble()

        # Add public function declarations:
        outfile.write("!! public interfaces", 0)
        outfile.write("public :: restart_physics_init", 1)
        outfile.write("public :: restart_physics_write", 1)
        outfile.write("public :: restart_physics_read", 1)

        all_req_vars = in_vars + out_vars
        # Grab the required variables that are also restart variables
        required_restart_vars, required_restart_constituents, constituent_dimmed_vars = gather_required_restart_variables(all_req_vars, constituent_set, restart_vars)

        outfile.blank_line()
        outfile.comment("Private module data", 0)
        for required_var in required_restart_vars:
            outfile.write(f"type(var_desc_t) :: {required_var.lower()}_desc", 1)
        # end for

        for required_var in constituent_dimmed_vars:
            outfile.write(f"type(var_desc_t), allocatable :: {required_var.lower()}_desc(:)", 1)
        # end for

        # Add "contains" statement:
        outfile.end_module_header()
        outfile.blank_line()

        # Write the restart init subroutine
        write_restart_physics_init(outfile, required_restart_vars, required_restart_constituents, constituent_dimmed_vars)

def write_restart_physics_init(outfile, required_vars, constituent_vars, constituent_dimmed_vars):
    """
    Write the init routine for the physics restart variables. This
    routine initializes the physics fields on the restart (cam.r) file
    """

    outfile.write("subroutine restart_physics_init(file, hdimids, errmsg, errflg)", 1)

    use_stmts = [["pio", ["file_desc_t", "pio_bcast_error",
                          "pio_set_errorhandling", "pio_double",
                          "pio_def_var"]],
                 ["cam_abortutils", ["endrun"]],
                 ["shr_kind_mod", ["SHR_KIND_CS, SHR_KIND_CL, SHR_KIND_CX"]],
                 ["cam_ccpp_cap", ["cam_model_const_properties"]],
                 ["ccpp_kinds", ["kind_phys"]],
                 ["cam_logfile", ["iulog"]],
                 ["spmd_utils", ["masterproc"]],
                 ["ccpp_constituent_prop_mod", ["ccpp_constituent_prop_ptr_t"]]]

    # Add in host model data use statements
#    use_stmts.extend(host_imports)

    # Output required, registered Fortran module use statements:
    write_use_statements(outfile, use_stmts, 2)

    # pio: pio_bcast_error, file_desc_t, pio_seterrorhandling, pio_double
    
    outfile.write("type(file_desc_t), intent(inout) :: file", 2)
    outfile.write("integer,           intent(in)    :: hdimids(:)", 2)
    outfile.write("character(len=512),intent(out)   :: errmsg", 2)
    outfile.write("integer,           intent(out)   :: errflg", 2)
    outfile.blank_line()

    # Local variables
    outfile.write("integer :: constituent_idx", 2)
    outfile.write("type(ccpp_constituent_prop_ptr_t), pointer :: const_props(:)", 2)
    outfile.write("character(len=512) :: const_locname", 2)

    outfile.blank_line()

    outfile.write("call pio_seterrorhandling(file, PIO_BCAST_ERROR)", 2)
    outfile.blank_line()

    outfile.comment("Define required restart variables on the restart file", 2)
    for required_var in required_vars:
        desc_name = f"{required_var.lower()}_desc"
        outfile.write(f"errflg = pio_def_var(file, '{required_var}', pio_double, hdimids, {desc_name})", 2)
        outfile.write("if (errflg /= 0) then", 2)
        outfile.write(f"write(errmsg,*) 'restart_physics_init: error defining variable {required_var}'", 3)
        outfile.write("return", 3)
        outfile.write("end if", 2)
        outfile.blank_line()
    # end for

    # Handle constituent-dimensioned variables
    if constituent_dimmed_vars:
        outfile.write("const_props => cam_model_const_properties()", 2)
        outfile.blank_line()
        for required_var in constituent_dimmed_vars:
            outfile.comment(f"Handling for constituent-dimensioned variable '{required_var}'", 2)
            outfile.write(f"allocate({required_var.lower()}_desc(size(const_props)), stat=errflg, errmsg=errmsg)", 2)
            outfile.write("if (errflg /= 0) then", 2)
            outfile.write("return", 3)
            outfile.write("end if", 2)
            outfile.write("do constituent_idx = 1, size(const_props)", 2)
            outfile.comment("Grab constituent local name:", 3)
            outfile.write("call const_props(constituent_idx)%local_name(const_locname)", 3)
            desc_name = f"{required_var.lower()}_desc"
            outfile.write(f"errflg = pio_def_var(file, '{required_var}_'//trim(const_locname), pio_double, hdimids, {desc_name}(constituent_idx))", 3)
            outfile.write("if (errflg /= 0) then", 3)
            outfile.write(f"write(errmsg,*) 'restart_physics_init: error defining variable {required_var}_'//trim(const_locname)", 4)
            outfile.write("end if", 3)
            outfile.write("end do", 2)
            outfile.blank_line()
        # end for
    # end if


    outfile.write("end subroutine restart_physics_init", 1)

#################
#HELPER FUNCTIONS
#################

##############################################################################
def _find_and_add_host_variable(stdname, host_dict, var_dict):
    """Find <stdname> in <host_dict> and add it to <var_dict> if found and
          not of type, 'host'.
       If not found, add <stdname> to <missing_vars>.
       If found and added to <var_dict>, also process the standard names of
          any intrinsic sub-elements of <stdname>.
       Return the list of <missing_vars> (if any).
       Note: This function has a side effect (adding to <var_dict>).
    """
    missing_vars = []
    hvar = host_dict.find_variable(stdname)
    if hvar and (hvar.source.ptype != 'host'):
        var_dict[stdname] = hvar
        # Process elements (if any)
        ielem = hvar.intrinsic_elements()
        # List elements are the only ones we care about
        if isinstance(ielem, list):
            for sname in ielem:
                smissing = _find_and_add_host_variable(sname, host_dict,
                                                       var_dict)
                missing_vars.extend(smissing)
            # end for
        # end if
    # end if
    if not hvar:
        missing_vars.append(stdname)
    # end if
    return missing_vars

##############################################################################
def gather_ccpp_req_vars(cap_database, registry_constituents):
    """
    Generate a list of host-model and constituent variables
    required by the CCPP physics suites potentially being used
    in this model run.
    <cap_database> is the database object returned by capgen.
    It is an error if any physics suite variable is not accessible in
       the host model.
    Return several values:
    - A list of host model variables
    - An error message (blank for no error)
    """

    # Dictionary of all 'in' and 'inout' suite variables.
    # Key is standard name, value is host-model or constituent variable
    in_vars = {}
    out_vars = {}
    missing_vars = set()
    constituent_vars = set()
    retmsg = ""
    # Host model dictionary
    host_dict = cap_database.host_model_dict()

    # Create CCPP datatable required variables-listing object:
    # XXgoldyXX: Choose only some phases here?
    for phase in CCPP_STATE_MACH.transitions():
        for cvar in cap_database.call_list(phase).variable_list():
            stdname = cvar.get_prop_value('standard_name')
            intent = cvar.get_prop_value('intent')
            is_const = cvar.get_prop_value('advected') or cvar.get_prop_value('constituent')
            if ((intent in _INPUT_TYPES) and
                (stdname not in in_vars) and
                (stdname not in _EXCLUDED_STDNAMES)):
                if is_const:
                    #Add variable to constituent set:
                    constituent_vars.add(stdname)
                    #Add variable to required variable list if it's not a registry constituent
                    if stdname not in registry_constituents:
                        in_vars[stdname] = cvar
                    # end if
                else:
                    # We need to work with the host model version of this variable
                    missing = _find_and_add_host_variable(stdname, host_dict,
                                                          in_vars)
                    missing_vars.update(missing)
                # end if
            # end if (only input variables)
            if ((intent in _OUTPUT_TYPES) and
                  (stdname not in out_vars) and
                  (stdname not in _EXCLUDED_STDNAMES)):
                if not is_const:
                    missing = _find_and_add_host_variable(stdname, host_dict,
                                                          out_vars)
                    # do nothing with missing variables
                # end if
            # end if (only output variables)
        # end for (loop over call list)
    # end for (loop over phases)

    if missing_vars:
        mvlist = ', '.join(sorted(missing_vars))
        retmsg = f"Error: Missing required host variables: {mvlist}"
    # end if
    # Return the required variables as a list
    return list(in_vars.values()), list(out_vars.values()), constituent_vars, retmsg

##############################################################################
def gather_required_restart_variables(all_req_vars, registry_constituents, restart_vars):
    """
    Return lists of the required restart non-constituent variables, and the required
    restart constituent variables
    """

    required_restart_vars = []
    required_constituent_restart_vars = []
    required_constituent_dimensioned_vars = []

    for required_var in all_req_vars:
        stdname = required_var.get_prop_value('standard_name')
        if stdname in restart_vars.keys():
            diagnostic_name = restart_vars[stdname]
            if stdname in registry_constituents:
                required_constituent_restart_vars.append(diagnostic_name)
            else:
                if 'ccpp_constant_one:number_of_ccpp_constituents' in required_var.get_dimensions():
                    print('HI THERE!')
                    required_constituent_dimensioned_vars.append(diagnostic_name)
                else:
                    required_restart_vars.append(diagnostic_name)
            # end if
        # end if (ignore all non-restart variables)
    # end for

    return required_restart_vars, required_constituent_restart_vars, required_constituent_dimensioned_vars

#####

def write_use_statements(outfile, use_stmts, indent):
    """Output Fortran module use (import) statements listed in <use_stmts>.
    """

    # The plus one is for a comma
    max_modname = max(len(x[0]) for x in use_stmts) + 1
    # max_modspace is the max chars of the module plus other 'use' statement
    #    syntax (e.g., 'only:')
    max_modspace = (outfile.indent_size * indent) + max_modname + 10
    mod_space = outfile.line_fill - max_modspace
    for use_item in use_stmts:
        # Break up imported interfaces to clean up use statements
        larg = 0
        num_imports = len(use_item[1])
        while larg < num_imports:
            int_str = use_item[1][larg]
            larg = larg + 1
            while ((larg < num_imports) and
                   ((len(int_str) + len(use_item[1][larg]) + 2) < mod_space)):
                int_str += f", {use_item[1][larg]}"
                larg = larg + 1
            # end while
            modname = use_item[0] + ','
            outfile.write(f"use {modname: <{max_modname}} only: {int_str}",
                          indent)
        # end while
    # end for

