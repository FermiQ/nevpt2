```markdown
# `infsym.F90` Module Documentation

## Module Name

`info_symmetry`

## Purpose

This Fortran module manages fundamental symmetry information for the molecular system and its orbitals within a quantum chemistry program (likely NEVPT2/CIPPI). Its primary responsibilities are:

1.  **Symmetry Multiplication Table:** To compute and store the direct product table for the irreducible representations (irreps) of the molecular point group.
2.  **Orbital Symmetry Assignment:** To assign an irrep label to each molecular orbital based on its type (inactive/core, active, secondary/virtual) and its symmetry block.

The module assumes an Abelian point group where the symmetry multiplication can be determined using bitwise operations (specifically XOR) on numerical labels of the irreps.

## Dependencies

*   `info_orbital_space`: It relies on data from this module (specifically `inforb%nish`, `inforb%nash`, `inforb%nssh` which are the number of inactive, active, and secondary orbitals per symmetry) to correctly populate the `itsym` array. It also updates total orbital counts (`nisht`, `nasht`, `nssht`) in the `inforb` structure.

## Public Module-Level Variables

The following arrays are declared at the module level and are publicly accessible after initialization.

| Variable   | Type                               | Description                                                                                                                                                                                             |
| :--------- | :--------------------------------- | :------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------ |
| `its(:,:)` | `INTEGER, ALLOCATABLE, PUBLIC`     | The symmetry multiplication table. `its(i,j)` stores the numerical label of the irrep that results from the direct product of irrep `i` and irrep `j`. Allocated as `(nsym, nsym)`.                       |
| `itsym(:)` | `INTEGER, ALLOCATABLE, PUBLIC`     | Array storing the symmetry label (irrep number) for each molecular orbital. Allocated as `(norb+norb+1)`, potentially to accommodate alpha and beta spin-orbitals and possibly a fictitious orbital. |

## Public Subroutines

---

### `initialize_infsym`

*   **Signature:** `subroutine initialize_infsym(nsym, norb)`
*   **Arguments:**
    *   `nsym`: `INTEGER, INTENT(IN)` - The number of irreducible representations in the point group.
    *   `norb`: `INTEGER, INTENT(IN)` - The total number of molecular orbitals.
*   **Purpose:**
    1.  Allocates the public module arrays `its` (to `nsym x nsym`) and `itsym` (to `norb + norb + 1`).
    2.  **Populates `its`:** Calculates the symmetry direct product table. It uses an internal statement function `muld2h(i,j) = IEOR(i-1, j-1) + 1`. This formula implies that irreps are 1-indexed and the direct product is found by performing a bitwise XOR on the 0-indexed versions of the irrep labels, then converting back to 1-indexed. This is standard for Abelian groups like D2h and its subgroups.
    3.  **Populates `itsym`:** Assigns a symmetry label to each molecular orbital. It iterates `i` from 1 to `nsym`. For each `i`, it assigns this symmetry label `i` sequentially to:
        *   `inforb%nish(i)` inactive orbitals.
        *   `inforb%nash(i)` active orbitals.
        *   `inforb%nssh(i)` secondary (virtual) orbitals.
        This assumes a global orbital ordering where orbitals are grouped by type (inactive, then active, then secondary) and then by symmetry block within each type.
    4.  Updates the total counts of inactive, active, and secondary orbitals (`inforb%nisht`, `inforb%nasht`, `inforb%nssht`) in the `inforb` structure (from the `info_orbital_space` module).
    5.  Includes a consistency check to ensure that the total number of orbitals assigned a symmetry matches the input `norb`.

---

### `finalize_infsym`

*   **Signature:** `subroutine finalize_infsym()`
*   **Arguments:** None.
*   **Purpose:** Deallocates the memory used by the public module arrays `its` and `itsym`. This is important for proper memory management when this symmetry information is no longer required.

---

**Note on Getter Functions from Prompt:**

The functions `get_irrep_label` and `get_sym_mult_table` mentioned in the user prompt are not explicitly defined as separate getter routines in this module.
*   The symmetry label for each orbital can be accessed directly from the public array `itsym(:)` after it has been initialized.
*   The symmetry multiplication table can be accessed directly from the public array `its(:,:)` after it has been initialized.
```
