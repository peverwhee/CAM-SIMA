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

def write_physics_restart(cap_database, ic_names, registry_constituents,
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
        print(restart_vars)
        required_restart_vars, required_restart_constituents = gather_required_restart_variables(all_req_vars, constituent_set, restart_vars)

        # Add "contains" statement:
        outfile.end_module_header()
        outfile.blank_line()

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

    for restart_var in restart_vars:
        if restart_var in all_req_vars:
            if restart_var in registry_constituents:
                required_constituent_restart_vars.append(restart_var)
            else:
                required_restart_vars.append(restart_var)
            # end if
        # end if (ignore all non-required variables)
    # end for

    return required_restart_vars, required_constituent_restart_vars
