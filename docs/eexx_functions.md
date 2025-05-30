```markdown
# `eexx_functions.F90` Module Documentation

## Module Name

`eexx_functions`

## Purpose

This Fortran module provides a collection of functions primarily designed for quantum chemistry calculations, likely within the context of methods that explicitly use one-, two-, three-, and four-particle density matrices (or related quantities like cumulants) for an active space of orbitals. The functions seem to calculate specific contractions or transformations of these multi-particle terms, which are common in advanced electronic structure theories (e.g., n-electron valence state perturbation theory (NEVPT), cumulant theory, or density matrix-based methods).

The functions operate on multi-dimensional arrays representing these density matrices/terms (`d1`, `d2`, `d3`, `ro4`, `daaa`, `taa`, `dal`) and take orbital indices (`a,b,c,...`) as input. They often involve conditional additions of lower-order terms when input orbital indices are identical, which is characteristic of Wick's theorem applications or contractions of creation/annihilation operator strings.

## Dependencies

*   `indices_norm`: Used for obtaining packed indices (e.g., `ind_norm%lindi`) and `nwords` (likely related to storage of symmetric tensors).
*   `ord_utils`: Specifically uses the `ord8` subroutine, presumably for ordering or canonicalizing sets of eight orbital indices.
*   `omp_lib`: Suggests that OpenMP parallelization might be used within or in conjunction with these functions, although not explicitly shown in the function bodies themselves.

## Public Functions

The following functions are made public by the module:

---

### `eeee`

*   **Signature:** `real*8 function eeee(a,b,c,d,e,f,g,h, d3,d2,d1,ro4, nact,istate,metat)`
*   **Arguments:**
    *   `a,b,c,d,e,f,g,h`: `INTEGER, INTENT(IN)` - Input orbital indices.
    *   `d3`: `REAL*8, INTENT(IN), DIMENSION(metat,nact,nact,nact,nact,nact,nact)` - Input 3-particle tensor/density matrix component for the specified `istate`.
    *   `d2`: `REAL*8, INTENT(IN), DIMENSION(metat,nact,nact,nact,nact)` - Input 2-particle tensor/density matrix component for `istate`.
    *   `d1`: `REAL*8, INTENT(IN), DIMENSION(metat,nact,nact)` - Input 1-particle tensor/density matrix component for `istate`.
    *   `ro4`: `REAL*8, INTENT(IN), DIMENSION(nwords,nact,nact,nact,nact,metat)` - Input 4-particle tensor/density matrix component for `istate`.
    *   `nact`: `INTEGER, INTENT(IN)` - Number of active orbitals.
    *   `istate`: `INTEGER, INTENT(IN)` - Index of the current electronic state.
    *   `metat`: `INTEGER, INTENT(IN)` - Total number of electronic states being considered.
*   **Purpose:** Calculates a scalar value from a 4-particle term (`ro4`) and adds various contractions of 3-, 2-, and 1-particle terms (`d3`, `d2`, `d1`) based on coincidences among the input orbital indices (`b,c,d,e,f,g`). The comment "four-particle density matrix dealt with here" confirms its primary nature. It uses `ord8` to canonicalize the input indices before accessing `ro4`.

---

### `eeeet`

*   **Signature:** `real*8 function eeeet(a,b,c,d,e,f,g,h, d3,d2,d1,ro4, nact,istate,metat)`
*   **Arguments:** Same as `eeee`.
*   **Purpose:** Computes a transformed version of the quantity calculated by `eeee`. It calls `eeee` with permuted indices (`d,c` instead of `c,d`) and subtracts this from a term. If indices `c` and `d` are equal, it adds a term involving `eee2(a,b,e,f,g,h,...)`, which implies a specific type of antisymmetrization or transformation rule.

---

### `eee2`

*   **Signature:** `real*8 function eee2(a,b,c,d,e,f, daaa,taa,dal, nact,istate,metat)`
*   **Arguments:**
    *   `a,b,c,d,e,f`: `INTEGER, INTENT(IN)` - Input orbital indices.
    *   `daaa`: `REAL*8, INTENT(IN), DIMENSION(metat,nact,nact,nact,nact,nact,nact)` - Input 3-particle density matrix component for `istate`.
    *   `taa`: `REAL*8, INTENT(IN), DIMENSION(metat,nact,nact,nact,nact)` - Input 2-particle density matrix component for `istate`.
    *   `dal`: `REAL*8, INTENT(IN), DIMENSION(metat,nact,nact)` - Input 1-particle density matrix component for `istate`.
    *   `nact, istate, metat`: `INTEGER, INTENT(IN)`.
*   **Purpose:** Calculates a scalar value from a 3-particle density matrix (`daaa`) and adds contractions of 2- and 1-particle density matrices (`taa`, `dal`) based on coincidences among input orbital indices.

---

### `eee2t`

*   **Signature:** `real*8 function eee2t(a,b,c,d,e,f, daaa,taa,dal, nact,istate,metat)`
*   **Arguments:** Same as `eee2`.
*   **Purpose:** Computes a transformed version of the quantity calculated by `eee2`. It calls `eee2` with permuted indices (`d,c` for `c,d`) and subtracts. If `c.eq.d`, it adds a term involving `ee2(a,b,e,f,...)`.

---

### `eee2tl`

*   **Signature:** `real*8 function eee2tl(a,b,c,d,e,f, daaa,taa,dal, nact,istate,metat)`
*   **Arguments:** Same as `eee2`.
*   **Purpose:** Computes another transformed version of `eee2`. It calls `eee2` with permuted indices (`b,a` for `a,b`) and subtracts. If `a.eq.b`, it adds a term involving `ee2(c,d,e,f,...)`.

---

### `ee2`

*   **Signature:** `real*8 function ee2(a,b,c,d, taa,dal, nact,istate,metat)`
*   **Arguments:**
    *   `a,b,c,d`: `INTEGER, INTENT(IN)` - Input orbital indices.
    *   `taa`: `REAL*8, INTENT(IN), DIMENSION(metat,nact,nact,nact,nact)` - Input 2-particle density matrix component.
    *   `dal`: `REAL*8, INTENT(IN), DIMENSION(metat,nact,nact)` - Input 1-particle density matrix component.
    *   `nact, istate, metat`: `INTEGER, INTENT(IN)`.
*   **Purpose:** Calculates a scalar value from a 2-particle density matrix (`taa`) and adds a contraction of the 1-particle density matrix (`dal`) if indices `b` and `c` are equal.

---

### `ee2t`

*   **Signature:** `real*8 function ee2t(a,b,c,d, taa,dal, nact,istate,metat)`
*   **Arguments:** Same as `ee2`.
*   **Purpose:** Computes a transformed version of `ee2`. It calls `ee2` with permuted indices (`b,a` for `a,b`) and subtracts. If `a.eq.b`, it adds a scaled `dal` term.

---

### `ee2tr`

*   **Signature:** `real*8 function ee2tr(a,b,c,d, taa,dal, nact,istate,metat)`
*   **Arguments:** Same as `ee2`.
*   **Purpose:** Computes another transformed version of `ee2`. It calls `ee2` with permuted indices (`d,c` for `c,d`) and subtracts. If `c.eq.d`, it adds a scaled `dal` term.

---

### `ro3t`

*   **Signature:** `real*8 function ro3t(a,b,c,ap,bp,cp, d3,d2,d1, nact,istate,metat)`
*   **Arguments:**
    *   `a,b,c,ap,bp,cp`: `INTEGER, INTENT(IN)` - Input orbital indices (likely 'bra' and 'ket' sets).
    *   `d3, d2, d1`: `REAL*8, INTENT(IN)` - Components of 3-, 2-, and 1-particle density matrices (or related tensors) for the specified `istate`. Dimensions are as in `eeee`.
    *   `nact, istate, metat`: `INTEGER, INTENT(IN)`.
*   **Purpose:** Calculates a transformed 3-particle term. It starts with `-d3(istate,ap,bp,cp,a,b,c)` and adds numerous 2-particle (`d2`) and 1-particle (`d1`) terms, scaled by factors of 2 or -1 or -4, conditional on various equalities between the `a,b,c` and `ap,bp,cp` indices. This is characteristic of evaluating matrix elements or transforming density matrix elements between different representations or bases.

---

### `ro2t`

*   **Signature:** `real*8 function ro2t(ap,bp,a,b, d2,d1, nact,istate,metat)`
*   **Arguments:**
    *   `ap,bp,a,b`: `INTEGER, INTENT(IN)` - Input orbital indices.
    *   `d2, d1`: `REAL*8, INTENT(IN)` - Components of 2- and 1-particle density matrices for the specified `istate`. Dimensions are as in `eeee`.
    *   `nact, istate, metat`: `INTEGER, INTENT(IN)`.
*   **Purpose:** Calculates a transformed 2-particle term. Similar to `ro3t`, it starts with `d2(istate,a,b,ap,bp)` and adds various scaled `d1` terms based on equalities between the input indices.

---
```
