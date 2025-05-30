```markdown
# `ijkl_utils.F90` Module Documentation

## Module Name

`ijkl_utils`

## Purpose

This Fortran module provides a suite of utilities for managing, storing, reading, and retrieving one- and two-electron integrals required for quantum chemistry calculations, particularly within the NEVPT2 (n-electron valence state perturbation theory) program. It supports both traditional storage of two-electron integrals and on-the-fly calculation from Cholesky decomposition vectors.

The module defines a derived type `integrals` (exposed via the public module variable `nevpt_ijkl`) to encapsulate all integral data and associated indexing information.

## Dependencies

*   `hdf5_utils`: Used for reading Cholesky vectors and other data from HDF5 files.
*   `info_symmetry`: Provides symmetry information (e.g., `ITSYM` array for orbital symmetries, `ITS` for symmetry multiplication table).
*   `info_orbital_space`: Provides details about the orbital space, such as the number of orbitals per symmetry (`inforb%nosh`) and symmetry-blocked orbital start indices (`inforb%iosh`).
*   `nevpt2_cfg`: Provides global configuration flags, notably `Do_Cholesky`, which dictates the integral handling strategy.
*   `omp_lib`: Included for OpenMP support, though not explicitly used in the provided snippets.

## Derived Type: `integrals`

This type, publicly available as `nevpt_ijkl`, is used to store integral data and associated lookup tables.

| Member           | Type                        | Description                                                                 |
| :--------------- | :-------------------------- | :-------------------------------------------------------------------------- |
| `ntwoint`        | `INTEGER*8`                 | Total number of two-electron integrals (if stored traditionally).             |
| `noneint`        | `INTEGER`                   | Number of one-electron integrals.                                           |
| `ni4`            | `INTEGER*8`                 | Dimension of the two-electron integral array (if stored traditionally).     |
| `nad`            | `INTEGER, ALLOCATABLE(:)`   | Indexing array related to symmetry sectors for integrals.                   |
| `kt`             | `INTEGER, ALLOCATABLE(:)`   | Indexing array related to symmetry types for integrals.                     |
| `ndeb`           | `INTEGER, ALLOCATABLE(:)`   | Array storing offsets for different symmetry blocks of integrals.           |
| `num`            | `INTEGER, ALLOCATABLE(:)`   | Lookup table for packed triangular indices `(i*(i-1)/2 + j)`.               |
| `indic`          | `INTEGER, ALLOCATABLE(:)`   | Stores the symmetry of an orbital pair `(i,j)`.                             |
| `jndic`          | `INTEGER, ALLOCATABLE(:)`   | Stores the sequential index of an orbital pair `(i,j)` within its symmetry. |
| `lndic`          | `INTEGER, ALLOCATABLE(:)`   | (Purpose not fully clear from snippets, likely another indexing array).     |
| `twoint`         | `REAL*8, ALLOCATABLE(:)`    | Array to store two-electron integrals if not using Cholesky decomposition.  |
| `oneint`         | `REAL*8, ALLOCATABLE(:)`    | Array to store one-electron integrals.                                      |
| `cholesky_array` | `REAL*8, ALLOCATABLE(:,:)`  | Array to store Cholesky vectors if `Do_Cholesky` is true.                   |

## Public Functions and Subroutines

---

### `initialize_ijkl`

*   **Signature:** `subroutine initialize_ijkl(n1int, norb)`
*   **Arguments:**
    *   `n1int`: `INTEGER, INTENT(IN)` - Number of one-electron integrals.
    *   `norb`: `INTEGER, INTENT(IN)` - Number of orbitals.
*   **Purpose:** Allocates and initializes to zero the arrays within the `nevpt_ijkl` module variable (e.g., `oneint`, `twoint` if not Cholesky, `num`, `indic`, `jndic`, `lndic`). Sets `noneint` and `ntwoint`.

---

### `finalize_ijkl`

*   **Signature:** `subroutine finalize_ijkl()`
*   **Arguments:** None.
*   **Purpose:** Deallocates all allocatable arrays within `nevpt_ijkl`. If Cholesky decomposition was used, it also closes the HDF5 dataset and dataspace for Cholesky vectors.

---

### `determine_n2int`

*   **Signature:** `subroutine determine_n2int(norb, nsym)`
*   **Arguments:**
    *   `norb`: `INTEGER, INTENT(IN)` - Number of orbitals.
    *   `nsym`: `INTEGER, INTENT(IN)` - Number of symmetries.
*   **Purpose:** Calculates the total number of unique two-electron integrals (`nevpt_ijkl%ni4`) considering orbital symmetry. It populates the `nevpt_ijkl%ndeb`, `nevpt_ijkl%nad`, and `nevpt_ijkl%kt` arrays, which are part of the complex indexing scheme used to locate integrals based on the symmetries of the four orbitals involved.

---

### `reijkl`

*   **Signature:** `subroutine reijkl(norb, nsym)`
*   **Arguments:**
    *   `norb`: `INTEGER, INTENT(IN)` - Number of orbitals.
    *   `nsym`: `INTEGER, INTENT(IN)` - Number of symmetries.
*   **Purpose:** Populates the primary indexing arrays `nevpt_ijkl%num` (for packed triangular indices), `nevpt_ijkl%indic` (symmetry of orbital pair `ij`), and `nevpt_ijkl%jndic` (sequential index of pair `ij` within its symmetry class). These are fundamental for mapping `(i,j,k,l)` orbital indices to a storage location for the integral `(ij|kl)`.

---

### `readint`

*   **Signature:** `subroutine readint(norb, nsym, nord_mol2nev, ecore)`
*   **Arguments:**
    *   `norb`: `INTEGER, INTENT(IN)` - Total number of orbitals.
    *   `nsym`: `INTEGER, INTENT(IN)` - Number of irreducible representations (symmetries).
    *   `nord_mol2nev`: `INTEGER, INTENT(IN), ARRAY(*)` - Mapping array for orbital reordering from an external program (e.g., MOLCAS) to the NEVPT2 internal ordering.
    *   `ecore`: `REAL*8, INTENT(OUT)` - Core energy (read from HDF5, though commented out in snippet).
*   **Purpose:** A high-level driver routine for integral processing. It orchestrates:
    1.  Calling `determine_n2int` to calculate required array sizes for two-electron integrals.
    2.  Calling `initialize_ijkl` to allocate and zero-out integral arrays.
    3.  Calling `reijkl` to set up basic indexing arrays.
    4.  Calling `ext1int2nev1int` to read and store one-electron integrals.
    5.  If `Do_Cholesky` is false: Calls `ext2int2nev2int` to read and store all two-electron integrals.
    6.  If `Do_Cholesky` is true: Initializes HDF5 reading for Cholesky vectors and reads the entire Cholesky set into `nevpt_ijkl%cholesky_array`.

---

### `ext1int2nev1int`

*   **Signature:** `subroutine ext1int2nev1int(h1_out, nnashx, nr_orb_per_sym, nr_sym, nr_actorb_tot, ireost, lupri)`
*   **Arguments:**
    *   `h1_out`: `REAL*8, INTENT(OUT), ARRAY(nnashx)` - Array to store the reordered one-electron integrals.
    *   `nnashx`: `INTEGER, INTENT(IN)` - Total number of one-electron integrals (size of `h1_out`).
    *   `nr_orb_per_sym`: `INTEGER, INTENT(IN), ARRAY(*)` - Number of orbitals per symmetry.
    *   `nr_sym`: `INTEGER, INTENT(IN)` - Number of symmetries.
    *   `nr_actorb_tot`: `INTEGER, INTENT(IN)` - Total number of active orbitals (used for `ireost` dimension).
    *   `ireost`: `INTEGER, INTENT(IN), ARRAY(*)` - Orbital reordering map.
    *   `lupri`: `INTEGER, INTENT(IN)` - Logical unit for printing.
*   **Purpose:** Reads one-electron integrals from an HDF5 file (dataset "FockMO") and stores them into `h1_out` (`nevpt_ijkl%oneint`). It handles reordering of these integrals based on the `ireost` map and considers orbital symmetries.

---

### `ext2int2nev2int`

*   **Signature:** `subroutine ext2int2nev2int(norb, nr_orb_per_sym, iosh, nsym, ndeb, ireost, twoint, lupri)`
*   **Arguments:**
    *   `norb`: `INTEGER, INTENT(IN)` - Total number of orbitals.
    *   `nr_orb_per_sym`: `INTEGER, INTENT(IN), ARRAY(*)` - Number of orbitals per symmetry.
    *   `iosh`: `INTEGER, INTENT(IN), ARRAY(*)` - Starting index for orbitals of a given symmetry.
    *   `nsym`: `INTEGER, INTENT(IN)` - Number of symmetries.
    *   `ndeb`: `INTEGER, INTENT(IN), ARRAY(*)` - Offsets for storing integrals of different symmetry blocks.
    *   `ireost`: `INTEGER, INTENT(IN), ARRAY(*)` - Orbital reordering map.
    *   `twoint`: `REAL*8, INTENT(OUT), ARRAY(*)` - Array to store the two-electron integrals (`nevpt_ijkl%twoint`).
    *   `lupri`: `INTEGER, INTENT(IN)` - Logical unit for printing.
*   **Purpose:** Reads two-electron integrals from an HDF5 file. Integrals are expected to be batched by the symmetries of the four orbitals (p,q,r,s). It then uses the complex indexing scheme (derived from `reijkl` and `determine_n2int`, involving `nevpt_ijkl%num`, `nevpt_ijkl%indic`, `nevpt_ijkl%jndic`, and `ndeb`) to store these integrals into the `twoint` array in the canonical NEVPT2 order.

---

### `ai`

*   **Signature:** `real*8 function ai(i,j,k,l)`
*   **Arguments:**
    *   `i, j, k, l`: `INTEGER, INTENT(IN)` - Orbital indices for the two-electron integral `(ij|kl)`.
*   **Purpose:** This is the primary public interface for retrieving a two-electron integral. It acts as a wrapper and, based on the global `Do_Cholesky` flag, calls either:
    *   `ai_cholesky(i,j,k,l)`: If Cholesky decomposition is used.
    *   `ai_twoint(i,j,k,l)`: If traditional integral storage is used.

---

### `ai_cholesky` (Private, called by `ai`)

*   **Signature:** `real*8 function ai_cholesky(i,j,k,l)`
*   **Arguments:**
    *   `i, j, k, l`: `INTEGER, VALUE` - Orbital indices.
*   **Purpose:** Calculates the two-electron integral `(ij|kl)` "on-the-fly" by taking the dot product of the corresponding Cholesky vectors. The Cholesky vectors are retrieved from the `nevpt_ijkl%cholesky_array`. It uses an internal function `indice(a,b)` to map the orbital pair `(a,b)` to the correct index in the `cholesky_array`.

---

### `ai_twoint` (Private, called by `ai`)

*   **Signature:** `real*8 function ai_twoint(i,j,k,l)`
*   **Arguments:**
    *   `i, j, k, l`: `INTEGER, INTENT(IN)` - Orbital indices.
*   **Purpose:** Retrieves a pre-calculated two-electron integral `(ij|kl)` from the `nevpt_ijkl%twoint` array. This function implements the complex logic to map the four orbital indices `(i,j,k,l)` (considering permutation and symmetry) to the correct linear index in the packed `twoint` array, using the helper arrays `nevpt_ijkl%num`, `nevpt_ijkl%indic`, `nevpt_ijkl%jndic`, `nevpt_ijkl%nad`, `nevpt_ijkl%kt`, and `nevpt_ijkl%ndeb`.

---

**Note on `PICK2`, `PICK4`, `PICK3`, `HANDY`, `ITIJKL`:**

The functions `PICK2`, `PICK4`, `PICK3`, and `HANDY`, which were mentioned in the prompt and are typically associated with older versions of CIPPI for compressed integral storage and retrieval, are **not** defined within the `ijkl_utils.F90` module. The integral retrieval logic in this module is handled by `ai_twoint` (for standard storage) or `ai_cholesky` (for Cholesky vectors). Similarly, the functionality of the older `ITIJKL` (integral reading and transformation) is now primarily managed by `readint`, `ext1int2nev1int`, and `ext2int2nev2int` using HDF5 files.
```
