# Constituents

## Introduction/Overview
Some definitions to start (as written by a non-scientist, so there is more nuance than this!):

- A **constituent** is a physical quantity or substance that exists in the atmosphere
- A constituent can be **advected**, which means it is moved through the atmosphere by some sort of dynamical method
    - The **dynamical core** (dycore) is the part of an atmosphere model (like CAM-SIMA) that advects the quantities over the underlying grid. Dynamical cores include:
        - *null/none* - the null dycore does nothing and is used in CAM-SIMA to validate physics schemes
        - *spectral element (SE)*: the only dycore currently implemented in CAM-SIMA
        - *finite-volume cubed-sphere (FV3)*: not currently implemented in CAM-SIMA
        - *model for prediction across scales (MPAS)*: in progress
- A constituent had additional properties, such as:
    - **water type**: can be "dry", "wet", or "moist" (we can convert between any of these quantities - "dry" means that it's the "amount" of that constituent with respect to dry air
    - **mixing ratio type**: can be "volume" or "mass"
        - volume mixing ratio: constituent values are a ratio of how much of the constituent exists per mole of air (units = mol mol-1)
        - mass mixing ratio: constituent values are the mass of the constituent per unit volume (kg m-3)
    - **molar mass**: Molar mass of a given quantity; used in converting between mass mixing ratio and volume mixing ratio
    - **thermodynamically active**: thermodynamically active constituents in CAM-SIMA can be found in `src/data/air_composition.F90`
    - **minimum value**: The scheme `qneg` will set constituent values that are less than the minimum to the minimum

## CAM-SIMA constituent handling
### Determining constituents
There are three ways to identify a quantity as a constituent in CAM-SIMA:

1. Constituent is provided by host (CAM-SIMA):
    - Host model constituents allow for constituents to be added as a constituent independent of the physics being run
    - Host constituents are added in `cam_register_constituents` (in `src/control/cam_comp.F90`). Currently, we are always adding water vapor as a host constituent because it is expected by the SE dycore.
1. Constituent is a build-time physics constituent in a CCPP-ized scheme:
    - If a quantity is known to be a constituent at build-time, it is identified in the metadata for the scheme with: `advected = True`
1. Constituent is a run-time physics constituent in a CCPP-ized scheme:
    - Sometimes, a scheme does not know what constituents it will require until run-time. In this case, an array of constituent properties (one for each needed constituent) is returned from the cam register phase. An example of how this works can be found in `src/physics/ncar_ccpp/musica/micm/micm.F90`

### Registering & Initializing Constituents
The registration and initializaiton of the constituent data array and the constituent properties object are done through calls to the generated CCPP cap.

- **cam_ccpp_register_constituents**: combines the three sources of constituents into one `ccpp_model_constituents_t` object
    - Called before the physics grid (which requires the number of constituents) is initialized
- **cam_ccpp_initialize_constituents**: initializes the data array within the `ccpp_model_constituents_t` object
    - Called after the physics grid is initialized (so we know the size of the array allocate)
    - The array is initialized to (columns, levels, number of constituents)

### Constituent Usage
Constituent values and properties can be accessed from the host side and from the physics in the following ways:

- Host side: constituents and properties can be accessed via the host model and dycore by way of the `cam_constituents.F90` module, which is an interface to the CCPP cap, which is in turn an interface to the constituents object
- Physics: the constituent array and/or the constituent properties object are passed into a scheme via the following metadata (local name and intent may vary):

```
[ q ]
  standard_name = ccpp_constituents
  units = none
  type = real | kind = kind_phys
  dimensions = (horizontal_loop_extent,vertical_layer_dimension,number_of_ccpp_constituents)
  intent = inout
[ const_props ]
  standard_name = ccpp_constituent_properties
  units = None
  type = ccpp_constituent_prop_ptr_t
  dimensions = (number_of_ccpp_constituents)
  intent = in
```

## CCPP Framework constituent handling
This section can be removed when constituents are documented in the CCPP Framework documentation.
### Constituent object (Fortran)
The constituent object (found in `$CAM-SIMA/ccpp_framework/src/ccpp_constituent_prop_ mod.F90`) is a flexible and extendable means of containing necessary constituent data for the framework. The primary object is `ccpp_model_constituents_t`.

![text](constituents-classes.PNG "CCPP Framework constituent object(s)")

This object, importantly, contains the following properties (for which there is metadata; CCPP standard name in parenthesis):

- `const_metadata` (ccpp_constituent_properties)
- `num_layer_vars` (number_of_ccpp_constituents)
- `num_advected_vars` (number_of_ccpp_advected_constituents)
- `vars_layer` (ccpp_constituents)

The `const_metadata` property is of type `ccpp_constituent_prop_ptr_t`, which contains a pointer to a `ccpp_constituent_properties_t` object, as depicted above. This object contains all of the constituent properties for the constituents in the constituents array, with the same indices as the constituents array.

The `ccpp_model_constituents_t` type also contains a hash table of constituent properties for more efficient searching, as well as several methods used by the generated cap code. Some methods are highlighted below:

- *new_field*: add a new constituent’s set of metadata fields to the hash table
- *lock_table*: lock the constituent hash table and initialize the constituent array
    - Initializes the constituent array to the default value specified for each constituent (min value held in constituent props array for each constituent)
    - Packs the advected constituents at the front of the constituents array
- *const_index*: retrieves the constituent index for a constituent (provided the standard name)

### Code generation (python)
The constituents-related code generation routines provide an interface to the constituents object. These routines can be found in `$CAM-SIMA/ccpp_framework/scripts/constituents.py`, primarily within the “write_host_routines” function. The (most often used) generated routines related to constituents are:

- <hostname\>_ccpp_register_constituents
- <hostname\>_ccpp_initialize_constituents
- <hostname\>_ccpp_number_constituents
- <hostname\>_constituents_array
- <hostname\>_model_const_properties
- <hostname\>_const_get_index

The routines above are generated during `./preview_namelists` or `./case.build` and can be found here: `$CASE/bld/atm/obj/ccpp/cam_ccpp_cap.F90`

