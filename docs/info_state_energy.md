```markdown
# `info_state_energy.F90` Module Documentation

## Module Name

`info_state_energy`

## Purpose

This Fortran module is responsible for managing and storing energy-related information for various electronic states within a quantum chemistry calculation, likely as part of a NEVPT2 (n-electron valence state perturbation theory) program. It centralizes data that was formerly in a `/VEC/` common block.

The module stores:
*   Reference energies for multiple states.
*   Second-order Epstein-Nesbet (EN) and Møller-Plesset (MP) perturbation energy corrections.
*   Norms of the first-order EN and MP wavefunctions.
*   Effective Hamiltonian matrices for Strongly Contracted (SC) and Partially Contracted (PC) NEVPT2 approaches.

It provides routines for initializing these storage arrays, deallocating them, and a specific routine to fetch reference state energies by parsing MOLCAS `RasOrb` files.

## Dependencies

*   `nevpt2_cfg`: Used in `deinit_energies` to deallocate `MultGroup%State`, suggesting an interaction with multi-state configuration data managed elsewhere.

## Public Module-Level Variables

The following arrays are declared at the module level and are publicly accessible. They are allocated and deallocated by `init_energies` and `deinit_energies`, respectively.

| Variable    | Type                               | Description                                                                                                   |
| :---------- | :--------------------------------- | :------------------------------------------------------------------------------------------------------------ |
| `e(:)`      | `REAL*8, ALLOCATABLE`              | Stores reference energies for each state (e.g., from CASSCF or CI).                                           |
| `e2en(:,:)` | `REAL*8, ALLOCATABLE`              | Stores Epstein-Nesbet second-order energy corrections. Dimensions are likely `(nstates, nstates_or_components)`. |
| `e2mp(:,:)` | `REAL*8, ALLOCATABLE`              | Stores Møller-Plesset second-order energy corrections. Dimensions are similar to `e2en`.                      |
| `psimp(:)`  | `REAL*8, ALLOCATABLE`              | Stores the squared norm of the first-order Møller-Plesset corrected wavefunction for each state.                |
| `psien(:)`  | `REAL*8, ALLOCATABLE`              | Stores the squared norm of the first-order Epstein-Nesbet corrected wavefunction for each state.                  |
| `heff_sc(:,:)`| `REAL*8, ALLOCATABLE`            | Stores the effective Hamiltonian matrix for the Strongly Contracted (SC) NEVPT2 method.                         |
| `heff_pc(:,:)`| `REAL*8, ALLOCATABLE`            | Stores the effective Hamiltonian matrix for the Partially Contracted (PC) NEVPT2 method.                        |

## Public Subroutines

---

### `init_energies`

*   **Signature:** `subroutine init_energies(nstates)`
*   **Arguments:**
    *   `nstates`: `INTEGER, INTENT(IN)` - The number of electronic states for which energy information will be stored.
*   **Purpose:** Allocates memory for all public module-level arrays (`e`, `e2en`, `e2mp`, `psimp`, `psien`, `heff_sc`, `heff_pc`). The dimensions are set based on `nstates`. All array elements are initialized to zero.

---

### `deinit_energies`

*   **Signature:** `subroutine deinit_energies()`
*   **Arguments:** None.
*   **Purpose:** Deallocates the memory previously allocated for the public module-level energy arrays. It also deallocates `MultGroup%State` from the `nevpt2_cfg` module.

---

### `get_state_energies`

*   **Signature:** `subroutine get_state_energies(energies, nstates, state_ptr)`
*   **Arguments:**
    *   `energies`: `REAL*8, INTENT(OUT), ARRAY(nstates)` - An array where the retrieved reference energies will be stored.
    *   `nstates`: `INTEGER, INTENT(IN)` - The number of states whose energies are to be retrieved.
    *   `state_ptr`: `INTEGER, INTENT(IN), ARRAY(nstates)` - An array that maps the desired local state indices (1 to `nstates`) to the corresponding state numbers used in `RasOrb` filenames (e.g., `state_ptr(1) = 2` means the first energy to get is for state 2).
*   **Purpose:** This subroutine reads reference (presumably CASSCF) energies for specified electronic states from MOLCAS `RasOrb` output files.
    1.  It iterates from `m = 1` to `nstates`.
    2.  For each `m`, it determines the target state number `s = state_ptr(m)`.
    3.  It constructs a filename pattern like `*.RasOrb.s` (e.g., `*.RasOrb.1`, `*.RasOrb.2`).
    4.  It uses a system command `ls *.RasOrb.s > STATE_GSS_NEVPT` to find the exact filename and writes it to a temporary file `STATE_GSS_NEVPT`.
    5.  It reads the actual `RasOrb` filename from `STATE_GSS_NEVPT`.
    6.  It opens the identified `RasOrb` file and searches for lines starting with `#INFO`.
    7.  It parses these `#INFO` lines to find an energy value (typically following an `=` sign).
    8.  The extracted energy is stored in `energies(m)`.
*   **Note:** The routine includes a comment that if called from within MOLCAS, it might be preferable to read energies directly from the `JobIph` file instead of parsing `RasOrb` files.

---

**Note on Other Functions from Prompt:**

The functions `get_nr_states`, `get_gs_energy`, `get_state_energy`, `get_state_weight`, `set_state_energy`, `set_state_weight` mentioned in the user prompt are **not** defined in this specific module (`info_state_energy.F90`). Access to energies and related data is primarily through direct access to the public module-level arrays after they are filled (e.g., by `init_energies` and subsequent calculations in other parts of the NEVPT2 program, or by `get_state_energies` for reference energies).
```
