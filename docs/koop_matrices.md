```markdown
# `koop_matrices.F90` Module Documentation

## Module Name

`koop_matrices`

## Purpose

This Fortran module is responsible for the construction and management of various Koopmans-like matrices and related quantities. These matrices are essential components for n-electron valence state perturbation theory (NEVPT2), particularly for calculating terms related to ionization potentials (IPs), electron affinities (EAs), and semi-internal excitations involving the active space.

The module takes density matrices (1-particle, 2-particle, 3-particle) and integrals (one-electron, two-electron) as input, computes the necessary Koopmans matrices, and writes them to an HDF5 file for use in subsequent NEVPT2 calculations.

## Dependencies

*   `eexx_functions`: Uses functions from this module (e.g., `ro2t`) for transforming or calculating density matrix components.
*   `info_symmetry`: Implicitly used via `eexx_functions` or for symmetry considerations in orbital indexing (though not directly shown in top-level calls within this module's snippet).
*   `hdf5_utils`: Used extensively to write the computed matrices (Koopmans matrices, density matrices) to an HDF5 output file, typically accessed via `file_id(1)`.

## Public Subroutines

---

### `koop_matrices_ctl`

*   **Signature:** `subroutine koop_matrices_ctl(metat, nact, ncore, atwo, daaa, taa, dal, f, aj)`
*   **Arguments:**
    *   `metat`: `INTEGER, INTENT(IN)` - Number of electronic states being considered.
    *   `nact`: `INTEGER, INTENT(IN)` - Number of active orbitals.
    *   `ncore`: `INTEGER, INTENT(IN)` - Number of core (inactive doubly occupied) orbitals.
    *   `atwo(nact,nact,nact,nact)`: `REAL*8, INTENT(IN)` - Two-electron integrals `(ij|kl)` within the active space.
    *   `daaa(metat,nact,nact,nact,nact,nact,nact)`: `REAL*8, INTENT(IN)` - Three-particle density matrix of the active space for each state.
    *   `taa(metat,nact,nact,nact,nact)`: `REAL*8, INTENT(IN)` - Two-particle density matrix of the active space for each state.
    *   `dal(metat,nact,nact)`: `REAL*8, INTENT(IN)` - One-particle density matrix of the active space for each state.
    *   `f(*)`: `REAL*8, INTENT(INOUT)` - Array containing one-electron integrals (Fock matrix elements), potentially modified (e.g., to form an effective Hamiltonian).
    *   `aj(*)`: `REAL*8, INTENT(IN)` - Array containing Coulomb integrals `J_pq`.
*   **Purpose:** This is the main public entry point and control subroutine for the module. It orchestrates the calculation of all necessary Koopmans-like matrices by calling:
    1.  `koopE_driver`: To compute and store 1-electron Koopmans matrices and the 1-RDM.
    2.  `koop_matrices_driver`: To compute and store 2-electron Koopmans matrices, the 2-hole RDM, and generalized Koopmans matrices for semi-internal terms.

## Internal Driver Subroutines

These subroutines are called by `koop_matrices_ctl` or each other to manage specific parts of the Koopmans matrix generation and output.

---

### `koopE_driver`

*   **Signature:** `subroutine koopE_driver(atwo, taa, dal, nact, ncore, metat, f, aj)`
*   **Arguments:** Same as relevant parts of `koop_matrices_ctl`.
*   **Purpose:**
    1.  Calls `koopE` to compute the 1-electron IP-like (`coopipa`) and EA-like (`coopeaa`) Koopmans matrices.
    2.  Writes these matrices to HDF5 datasets named `"1-IPKO"` and `"1-EAKO"`.
    3.  Writes the 1-particle density matrix (`dal`) to HDF5 dataset `"1-RDM"`.

---

### `koop_matrices_driver`

*   **Signature:** `subroutine koop_matrices_driver(atwo, daaa, taa, dal, nact, ncore, metat, f, aj)`
*   **Arguments:** Same as relevant parts of `koop_matrices_ctl`.
*   **Purpose:**
    1.  Calculates the transformed 2-hole density matrix (`koopaa(istate,i,j,k,l) = ro2t(i,j,k,l,...)`) using the `ro2t` function from `eexx_functions`.
    2.  Writes this `koopaa` to HDF5 dataset `"2-HRDM"`.
    3.  Calls `koop2E_driver` to compute and store 2-electron Koopmans matrices.
    4.  Calls `koopman0pE_driver` to compute and store generalized Koopmans matrices for semi-internal (`V(0')`) terms.

---

### `koop2E_driver`

*   **Signature:** `subroutine koop2E_driver(atwo, daaa, taa, dal, koopaa, nact, ncore, metat, f, aj)`
*   **Arguments:**
    *   `koopaa`: `REAL*8, INTENT(INOUT)` - On input, the 2-hole RDM. On output (from `koop2E`), it's modified to be the 2-IP Koopmans matrix.
    *   Other arguments are as in `koop_matrices_driver`.
*   **Purpose:**
    1.  Calls `koop2E` to compute the 2-electron EA Koopmans matrix (`koopeaa`) and to modify the input `koopaa` into the 2-electron IP Koopmans matrix.
    2.  Writes `koopeaa` to HDF5 dataset `"2-EAKO"`.
    3.  Writes the modified `koopaa` to HDF5 dataset `"2-IPKO"`.
    4.  Writes the 2-particle density matrix (`taa`) to HDF5 dataset `"2-RDM"`.

---

### `koopman0pE_driver`

*   **Signature:** `subroutine koopman0pE_driver(atwo, daaa, taa, dal, koopaa, nact, ncore, metat, f, aj)`
*   **Arguments:**
    *   `koopaa`: `REAL*8, INTENT(INOUT)` - On input, 2-hole RDM. Modified by `koopman0pE` to become `gk0pa`.
    *   Other arguments are as in `koop_matrices_driver`.
*   **Purpose:**
    1.  Calls `koopman0pE` to compute the generalized Koopmans matrices `gk0pa` (from modifying `koopaa`), `gk0pd` (stored in `koopbb`), and `gk0pf` (stored in `koopab`). These are needed for the `V(0')` semi-internal NEVPT2 terms.
    2.  Calls `fdiff_F` to check for large elements in `koopab` (`gk0pf`).
    3.  Writes these matrices to HDF5 datasets `"gk0pa"`, `"gk0pd"`, and `"gk0pf"`.

## Core Computational Subroutines

These subroutines perform the actual calculation of the Koopmans-like matrices.

---

### `koopE`

*   **Signature:** `subroutine koopE(atwo, koopipa, koopeaa, taa, dal, nact, ncore, metat, f, aj)`
*   **Arguments:** (as per `koopE_driver`, with `koopipa` and `koopeaa` as `INTENT(OUT)`)
*   **Purpose:** Calculates the 1-electron Koopmans matrices:
    *   `koopipa(state, ap, a)`: Matrix elements for active-space ionization potentials. $\approx \langle \Psi_0 | a_a^\dagger [H, a_{ap}] | \Psi_0 \rangle$
    *   `koopeaa(state, ap, a)`: Matrix elements for active-space electron affinities. $\approx \langle \Psi_0 | a_{ap} [H, a_a^\dagger] | \Psi_0 \rangle$
    It uses an effective one-electron Hamiltonian (`f`, modified with active-space contributions) and two-electron integrals (`atwo`) along with 1- and 2-particle density matrices (`dal`, `taa`).

---

### `koop2E`

*   **Signature:** `subroutine koop2E(atwo, daaa, taa, dal, koopaa, koopeaa, nact, ncore, metat, f, aj)`
*   **Arguments:** (as per `koop2E_driver`, with `koopaa` as `INTENT(INOUT)` and `koopeaa` as `INTENT(OUT)`)
*   **Purpose:** Calculates the 2-electron Koopmans matrices:
    *   Modifies `koopaa(state, ap, bp, a, b)` to store matrix elements for active-space double ionization potentials.
    *   `koopeaa(state, ap, bp, a, b)`: Matrix elements for active-space double electron affinities.
    It uses an effective one-electron Hamiltonian (`f`) and various density matrices and integrals.

---

### `koopman0pE`

*   **Signature:** `subroutine koopman0pE(atwo, daaa, taa, dal, koopaa, koopbb, koopab, nact, ncore, metat, f, aj)`
*   **Arguments:** (as per `koopman0pE_driver`, with `koopaa` as `INTENT(INOUT)`, `koopbb`, `koopab` as `INTENT(OUT)`)
*   **Purpose:** Calculates generalized Koopmans matrices required for the semi-internal `V(0')` terms in NEVPT2:
    *   Modifies `koopaa` to store `gk0pa`.
    *   Stores `gk0pd` in `koopbb`.
    *   Stores `gk0pf` in `koopab`.
    These matrices involve complex contractions of density matrices and integrals.

---

### `fdiff_F`

*   **Signature:** `subroutine fdiff_F(koopab, nact, metat)`
*   **Arguments:**
    *   `koopab(metat,nact,nact)`: `REAL*8, INTENT(IN)` - The `gk0pf` matrix.
    *   `nact, metat`: `INTEGER, INTENT(IN)`.
*   **Purpose:** Checks if any elements in the `koopab` matrix (which corresponds to `gk0pf`) exceed a predefined `threshold` (1.0d-7). If so, it prints the maximum value found and its indices for the relevant state. This serves as a diagnostic check.

## HDF5 Output Datasets

The module writes the following datasets to the HDF5 file associated with `file_id(1)`:

*   `"1-EAKO"`: 1-electron Electron Affinity Koopmans matrix (`koopeaa` from `koopE`).
*   `"1-IPKO"`: 1-electron Ionization Potential Koopmans matrix (`coopipa` from `koopE`).
*   `"1-RDM"`: 1-particle density matrix (`dal`).
*   `"2-HRDM"`: 2-hole density matrix (`ro2t` results).
*   `"2-EAKO"`: 2-electron Electron Affinity Koopmans matrix (`koopeaa` from `koop2E`).
*   `"2-IPKO"`: 2-electron Ionization Potential Koopmans matrix (`koopaa` modified by `koop2E`).
*   `"2-RDM"`: 2-particle density matrix (`taa`).
*   `"gk0pa"`: Generalized Koopmans matrix A for V(0') terms.
*   `"gk0pd"`: Generalized Koopmans matrix D for V(0') terms.
*   `"gk0pf"`: Generalized Koopmans matrix F for V(0') terms.
```
