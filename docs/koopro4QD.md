```markdown
# `koopro4QD.F90` Module Documentation

## Module Name

`koopro4QD`

## Purpose

This Fortran module contains the primary driver subroutine `koopro4QD_driver` which orchestrates the calculation of various quantities required for Quasi-Degenerate N-Electron Valence State Perturbation Theory (QD-NEVPT2). This includes:

*   Calculation of state-specific 1- to 4-particle reduced density matrices (RDMs) for reference states, either from existing CI vectors or by interfacing with a DMRG program (QCMAQUIS).
*   Calculation of transition density matrices (TDMs) between reference states if a multi-state calculation is performed.
*   Computation of various Koopmans-like matrices using these RDMs and molecular integrals.
*   Construction of other intermediate matrices (referred to as A, B, C, D matrices) that likely form parts of the effective Hamiltonian in QD-NEVPT2.
*   Storing all computed RDMs, Koopmans matrices, and intermediate matrices into an HDF5 file (`nevpt.h5`) for subsequent use.

The module integrates functionalities from several other modules for tasks like orbital space definition, symmetry handling, integral reading, and the core Koopmans matrix calculations.

## Dependencies

The module `koopro4QD` has several dependencies:

*   `indices_norm`: For canonical indexing of 4-index quantities.
*   `koop_matrices`: Uses `koop_matrices_ctl` for the computation of state-specific Koopmans matrices.
*   `ord_utils`: Likely used by internal permutation and ordering helper subroutines.
*   `hdf5_utils`: For all HDF5 file operations (initialization, creation, opening, writing datasets).
*   `info_orbital_space`: For initializing and managing orbital space definitions (e.g., number of core, active, virtual orbitals per symmetry).
*   `info_symmetry`: For initializing and managing symmetry information (orbital symmetries, multiplication tables).
*   `nevpt2_cfg`: For global configuration parameters (e.g., number of states, thresholds, flags like `do_dmrg_pt`, `no_4rdm_terms`, file paths from `&LEGGI` if this module were a main program, but here it uses `nevpt2_cfg` directly).
*   `ijkl_utils`: For reading one- and two-electron integrals (`readint`, `get_aj_atwo`) and managing their storage.
*   `qcmaquis_interface`, `qcmaquis_interface_cfg`, `qcmaquis_interface_utility_routines`, `rdm_utils`: Used conditionally (`#ifdef DMRG_NEVPT`) for interfacing with the QCMAQUIS DMRG program to obtain RDMs and TDMs.

## Public Subroutines

---

### `koopro4QD_driver`

*   **Signature:** `subroutine koopro4QD_driver(rel_ham, t13, from_molcas)`
*   **Arguments:**
    *   `rel_ham`: `LOGICAL, INTENT(IN)` - Flag indicating if a relativistic Hamiltonian is used (its direct usage is not prominent in the provided snippet but may affect underlying calculations).
    *   `t13`: `REAL*4, INTENT(OUT)` - Returns the total CPU time for the execution of this subroutine.
    *   `from_molcas`: `LOGICAL, INTENT(IN)` - Flag indicating if the calling environment or data source is MOLCAS. This affects how orbital space information is initialized.
*   **Purpose:** This is the main orchestrating subroutine of the module. It performs the following major steps:
    1.  **Initialization:**
        *   Records initial CPU time.
        *   Initializes HDF5 utilities and creates/opens HDF5 files: `nevpt.h5` (for output, `file_id(1)`) and `ijkl.h5` (for input integrals, `file_id(2)`).
        *   Initializes orbital space information via `initialize_inforb` (from `info_orbital_space`), which reads data either from HDF5 (if not `from_molcas`) or uses data populated by a MOLCAS interface.
        *   Initializes symmetry information via `initialize_infsym` (from `info_symmetry`).
        *   Initializes 4-index quantity normalization data via `initialize_indices_norm` (from `indices_norm`).
    2.  **State-Specific Density Matrix Calculation:**
        *   If not using DMRG (`.not. do_dmrg_pt`): Reads CI vectors and determinant information from `FILE04` (specified in `nevpt2_cfg`). Processes this information using local routines (`esclass`, `giveocc`).
        *   Calls `state_specific_densities_driver` which:
            *   If `do_dmrg_pt` is true, interfaces with QCMAQUIS to obtain 1-, 2-, 3-, and optionally 4-particle RDMs for each state.
            *   If `do_dmrg_pt` is false, calls internal routines (`ro1`, `ro2`, `ro3`, `rofour`) to compute these RDMs from the CI vectors.
            *   Contracts higher-order RDMs to lower-order ones using `bro1`, `bro2`, `bro3`.
        *   Writes the 3-RDM (`daaa`) to the HDF5 file (`"3-RDM"` dataset).
    3.  **Integral Processing and Koopmans Matrix Calculation:**
        *   Calls `set_orbarray_pointers` (from `info_orbital_space`) to define orbital mappings.
        *   Calls `readint` (from `ijkl_utils`) to load one- and two-electron integrals.
        *   Calls `get_aj_atwo` (from `ijkl_utils`) to prepare active-space two-electron integrals (`atwo`) and effective one-electron integrals/Fock matrix elements (`aj`, `f`).
        *   Calls `koop_matrices_ctl` (from `koop_matrices`) which computes and writes various state-specific Koopmans matrices (1-IP/EA, 2-IP/EA, `gk0p` matrices) and also writes the 1-RDM, 2-RDM, and 2-HRDM to the HDF5 file.
        *   Deallocates integral arrays via `finalize_ijkl`.
    4.  **A, B, C, D Matrix Construction (Effective Hamiltonian components):**
        *   If `no_4rdm_terms` is false (i.e., 4-RDM is available and used):
            *   Calls `bamat` to compute `AMAT` and `ATMAT`. Writes these to HDF5 (`"amatIP"`, `"amatEA"`).
            *   Calls `bbmat` to compute `BMAT` and `BTMAT`. Writes these to HDF5 (`"bmatIP"`, `"bmatEA"`).
            *   Calls `bcmat` to compute `CMAT` and `CTMAT`. Writes these to HDF5 (`"cmatIP"`, `"cmatEA"`).
            *   Calls `fdiff` to check consistency between B/C and BT/CT matrices.
            *   Calls `bdmat` to compute `DMAT` and `DTMAT`. Writes these to HDF5 (`"dmatIP"`, `"dmatEA"`).
    5.  **Transition Density Matrix Calculation (for multi-state, if `ncoppie > 0` and `skip_effective_ham` is false):**
        *   Calls `transition_densities_driver` which:
            *   If `do_dmrg_pt` is true, interfaces with QCMAQUIS to obtain 1-, 2-, and 3-particle transition density matrices (TDMs).
            *   If `do_dmrg_pt` is false, calls internal routines (`ro1off`, `ro2off`, `ro3off`) to compute these TDMs from CI vectors.
            *   Contracts higher-order TDMs to lower-order ones.
        *   Writes the 1-TDM, 2-TDM, and 3-TDM to HDF5 datasets (`"1-TRDM"`, `"2-TRDM"`, `"3-TRDM"`).
    6.  **Finalization:**
        *   Deallocates remaining local arrays (CI vectors, determinant info if read).
        *   Calls `finalize_indices_norm`, `finalize_inforb`, `finalize_infsym`.
        *   Closes HDF5 files and finalizes HDF5 utilities.
        *   Records final CPU time into `t13`.

## Internal Helper and Computational Subroutines

The `koopro4QD_driver` calls several key subroutines defined within the same module or file for RDM calculations if not using DMRG:

*   **`state_specific_densities_driver`**: Orchestrates the calculation of state-specific RDMs.
*   **`transition_densities_driver`**: Orchestrates the calculation of transition RDMs.
*   **`rofour`, `ro3`, `ro2`, `ro1`**: Compute 4-, 3-, 2-, and 1-particle RDMs from CI expansion.
*   **`ro3off`, `ro2off`, `ro1off`**: Compute 3-, 2-, and 1-particle transition RDMs.
*   **`bro3`, `bro2`, `bro1`**: Perform contractions of RDMs (e.g., 4-RDM to 3-RDM).
*   **`bamat`, `bbmat`, `bcmat`, `bdmat`**: Compute specific A, B, C, D matrix components for the effective Hamiltonian.
*   Various permutation and ordering utilities (`ord*`, `permut*`, `giveocc2`, `jdifgen`, `accum`, `esclass`, `noverk`, `getnorm`, `lindiceold`).

## Input Data Summary

*   Configuration parameters from the `nevpt2_cfg` module.
*   If not using DMRG: CI vectors and determinant information from `FILE04`.
*   Molecular integrals (one- and two-electron) via `ijkl_utils` (reading from `ijkl.h5` which itself is populated from `FILE50`, `FILE25`, etc.).
*   If using DMRG: Checkpoint files from QCMAQUIS.

## Output Data Summary

All primary data is written to an HDF5 file named `nevpt.h5` (hardcoded filename for `file_id(1)`). Key datasets include:
*   State-specific RDMs: `"1-RDM"`, `"2-RDM"`, `"3-RDM"`, (optionally 4-RDM if not DMRG and `ZRO4` was true in `koopro4Ecl2.F`'s equivalent step).
*   Transition RDMs: `"1-TRDM"`, `"2-TRDM"`, `"3-TRDM"`.
*   Koopmans matrices (written by `koop_matrices_ctl`):
    *   `"1-IPKO"`, `"1-EAKO"`
    *   `"2-IPKO"`, `"2-EAKO"`
    *   `"2-HRDM"` (2-hole RDM)
    *   `"gk0pa"`, `"gk0pd"`, `"gk0pf"`
*   A, B, C, D matrix components (if `no_4rdm_terms` is false):
    *   `"amatIP"`, `"amatEA"`
    *   `"bmatIP"`, `"bmatEA"`
    *   `"cmatIP"`, `"cmatEA"`
    *   `"dmatIP"`, `"dmatEA"`

This module serves as a high-level driver to prepare all necessary many-body quantities for a subsequent QD-NEVPT2 energy calculation.
```
