```markdown
# `indices_norm.F90` Module Documentation

## Module Name

`indices_norm`

## Purpose

This Fortran module provides utilities for managing and calculating "normalized" or canonical packed indices for 4-dimensional quantities, such as four-particle density matrices or two-electron integrals `(ij|kl)` where permutational symmetry is exploited. The main goal is to derive a unique 1D index from four individual orbital (or similar) indices, typically by first sorting the indices into a canonical order and then applying a packing formula.

It precomputes offset arrays (`lindi`, `lindj`, `lindk`) and a table of normalized indices (`normind`) to facilitate this. The module also calculates `nwords`, representing the total number of unique elements in such a symmetric 4th rank tensor.

## Dependencies

*   `swap_util`: Used by the normalization functions (`norm_array`, `norm_array32`) to sort orbital indices into a canonical (descending) order.
*   `omp_lib`: Presence suggests potential for OpenMP parallelization, though not directly visible in the provided code snippets for these specific functions.

## Public Derived Type: `type_ind_norm`

This derived type is used to group the precomputed indexing arrays. A module-level variable `ind_norm` of this type is publicly available and stores the actual tables.

| Member        | Type                                  | Description                                                                                                |
| :------------ | :------------------------------------ | :--------------------------------------------------------------------------------------------------------- |
| `lindi(:)`    | `INTEGER, PUBLIC, ALLOCATABLE`        | Stores precomputed offsets based on the first index `i`. Formula: `(i-1)*i*(i+1)*(i+2)/24`.                  |
| `lindj(:)`    | `INTEGER, PUBLIC, ALLOCATABLE`        | Stores precomputed offsets based on the second index `j`. Formula: `(j-1)*j*(j+1)/6`.                 |
| `lindk(:)`    | `INTEGER, PUBLIC, ALLOCATABLE`        | Stores precomputed offsets based on the third index `k`. Formula: `(k-1)*k/2`.                      |
| `normind(:,:,:,:)` | `INTEGER, PUBLIC, ALLOCATABLE`    | A 4D array storing pre-calculated normalized indices `norm(i,j,k,l)` for all combinations up to `nasht`. |

## Public Module Variable

### `ind_norm`

*   **Type:** `type_ind_norm`
*   **Attributes:** `SAVE`
*   **Purpose:** An instance of `type_ind_norm` that holds the actual precomputed indexing tables (`lindi`, `lindj`, `lindk`, `normind`). The `SAVE` attribute ensures these tables persist throughout the execution of the program after initialization.

## Public Parameters

### `nwords`

*   **Type:** `INTEGER, PUBLIC`
*   **Purpose:** Stores the total number of unique elements in a symmetric 4th rank tensor given `nasht` (the maximum value for an index).
*   **Calculation:** `nasht * (nasht + 1) * (nasht + 2) * (nasht + 3) / 24`. This corresponds to the number of unique elements `i >= j >= k >= l`.

## Public Subroutines and Functions

---

### `initialize_indices_norm`

*   **Signature:** `subroutine initialize_indices_norm(nasht)`
*   **Arguments:**
    *   `nasht`: `INTEGER, INTENT(IN)` - The maximum dimension or range of the individual indices (e.g., number of active orbitals).
*   **Purpose:**
    1.  Allocates the arrays `lindi`, `lindj`, `lindk`, and `normind` within the `ind_norm` module variable, with dimensions based on `nasht`.
    2.  Populates `lindi`, `lindj`, and `lindk` with precomputed offset values using their respective formulas.
    3.  Fills the `normind(i,j,k,l)` table by calling the `norm` function (which resolves to `norm_int`) for all combinations of indices up to `nasht`.
    4.  Calculates and stores the total number of unique packed indices in the public parameter `nwords`.

---

### `finalize_indices_norm`

*   **Signature:** `subroutine finalize_indices_norm()`
*   **Arguments:** None.
*   **Purpose:** Deallocates the memory used by the `lindi`, `lindj`, `lindk`, and `normind` arrays within the `ind_norm` module variable. This is important for memory management when these tables are no longer needed.

---

### `lindice`

*   **Signature:** `integer function lindice(i,j,k,l)`
*   **Arguments:**
    *   `i, j, k, l`: `INTEGER, INTENT(IN)` - The four individual indices.
*   **Purpose:** Calculates a packed 1D index based on the four input indices. It uses the precomputed offset arrays `ind_norm%lindi`, `ind_norm%lindj`, `ind_norm%lindk` and the last index `l`.
*   **Note:** This function assumes that the input indices `(i,j,k,l)` are already in the canonical order (e.g., `i >= j >= k >= l`) for which the packing formula is designed. For arbitrary indices, the `norm` interface should be used first.

---

### `norm` (Generic Interface)

This generic interface allows calling a normalization function that computes a canonical packed index from four input indices. The specific function called depends on the arguments provided.

*   **Specific Functions implementing `norm`:**

    *   **`norm_int(ia, ic, ie, ig)`**
        *   **Signature:** `integer function norm_int(ia, ic, ie, ig)`
        *   **Arguments:** `ia, ic, ie, ig`: `INTEGER, INTENT(IN)` - Four integer indices.
        *   **Purpose:** Acts as a wrapper for `norm_array` by taking individual integer arguments and passing them as an array.

    *   **`norm_array(nv_in)`**
        *   **Signature:** `integer function norm_array(nv_in)`
        *   **Arguments:** `nv_in`: `INTEGER, DIMENSION(4), INTENT(IN)` - An array containing four integer indices.
        *   **Purpose:**
            1.  Creates a local copy `nv` of the input array `nv_in`.
            2.  Sorts the indices in `nv` into descending order (e.g., `nv(1) >= nv(2) >= nv(3) >= nv(4)`) using a series of comparisons and calls to an external `swap` subroutine.
            3.  Calculates and returns a canonical packed 1D index using the sorted indices and the formula:
                `(nv(1)-1)*nv(1)*(nv(1)+1)*(nv(1)+2)/24 + (nv(2)-1)*nv(2)*(nv(2)+1)/6 + nv(3)*(nv(3)-1)/2 + nv(4)`.

    *   **`norm_int32(ia, ic, ie, ig)`**
        *   **Signature:** `integer*4 function norm_int32(ia, ic, ie, ig)`
        *   **Arguments:** `ia, ic, ie, ig`: `INTEGER*4, INTENT(IN)` - Four 4-byte integer indices.
        *   **Purpose:** Same as `norm_int`, but for `INTEGER*4` arguments. Calls `norm_array32`.

    *   **`norm_array32(nv_in)`**
        *   **Signature:** `integer*4 function norm_array32(nv_in)`
        *   **Arguments:** `nv_in`: `INTEGER*4, DIMENSION(4), INTENT(IN)` - An array containing four 4-byte integer indices.
        *   **Purpose:** Same as `norm_array`, but for `INTEGER*4` arguments and return type. Uses the same sorting logic and packing formula.

---
```
