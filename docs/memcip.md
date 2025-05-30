```markdown
# `memcip.F` - Memory Management Routines Documentation

## File Overview

The file `memcip.F` contains a set of Fortran subroutines (`MEMCIP`, `MEMDIG`, `MEMROS`) and a function (`NWRD`) designed to provide a form of dynamic memory management for older Fortran programs, particularly the CIPSI/CIPPI suite. In an environment where dynamic allocation might not be as flexible or standardized as in modern Fortran, these routines manage a large, pre-allocated (or requested) common block array (`X` in `COMMON /BIG/`) by dividing it into segments for different parts of a calculation.

The behavior of these routines, especially `MEMCIP`, can differ based on compiler flags (e.g., `INTEL`) which toggle between using Fortran's `allocate`/`deallocate` for some arrays and a more traditional approach of managing a static common block via `MEMGET` (a non-standard, system-dependent memory allocation function).

## Key Common Blocks

*   **`/MEM/`**:
    *   `MEMSIZ`: `INTEGER` - Stores the total size (in double precision words) of the memory pool currently allocated or available.
    *   `LOFFS`: `INTEGER` - An offset (in words) from the beginning of the `/BIG/` common block's `X` array to the actual start of the usable memory pool. This is used to align data correctly, especially in non-INTEL versions.
    *   `LOCMEM`: `INTEGER` - In non-INTEL versions, stores the starting address returned by `MEMGET`.
    *   `LOCM(10)`: `INTEGER` array - Stores the starting offsets (in words, relative to `X(LOFFS)`) for up to 10 different data arrays or sub-blocks required by a specific calculation stage (`ACTION` in `MEMCIP`).

*   **`/BIG/`**:
    *   Non-INTEL version: `X(1)` - Declared as a single element but acts as the base for a large contiguous memory block managed by `MEMGET`.
    *   INTEL version: `pointer x; dimension x(:)` - `x` is a Fortran pointer that is dynamically allocated. This common block might also include pointers to integral arrays (`ijkl2`, `ijkl4`, etc.) which are also dynamically allocated in this version.

*   **`/INT/`**:
    *   `NI4, NI3, NI2, NR4`: `INTEGER` - Store the number of two-electron integrals of different types (4-byte integer, 3-byte character, 2-byte integer, 4-byte real, respectively). This information is crucial for calculating memory requirements for integral storage.

*   **`/CIP/`, `/DIA/`, `/DOR/`**: These common blocks contain various program parameters (like `NCF`, `MAXM`, `NREFMO`, etc.) that `MEMCIP` uses to determine the memory needs for specific computational tasks.

## Subroutines and Functions

---

### `MEMCIP`

*   **Signature (INTEL version):** `SUBROUTINE MEMCIP(ACTION, doit)`
*   **Signature (Non-INTEL version):** `SUBROUTINE MEMCIP(ACTION)`
*   **Arguments:**
    *   `ACTION`: `CHARACTER*5, INTENT(IN)` - A code specifying the current stage of the calculation or the memory action to be performed.
    *   `doit`: `LOGICAL, INTENT(IN)` (INTEL version only) - If present, controls whether actual allocation/deallocation of specific integral arrays (like `ijkl2`) within `/BIG/` should occur.
*   **Purpose:** The primary memory management routine. It calculates memory requirements (`IWANT`) based on the `ACTION` and partitions the large array `X` (from `/BIG/`) using offsets stored in `LOCM`.
*   **Key `ACTION` codes and their behavior:**
    *   **`'RELSE'`**: Releases allocated memory.
        *   Non-INTEL: Calls `MEMREL(LOCMEM)` if `NOREL` is false. `MEMREL` is a non-standard deallocation routine.
        *   INTEL: Deallocates the pointer `x`. If `doit` is true, also deallocates `ijkl2`, `ijkl3`, `ijkl4`, `rijkl`.
    *   **`'TOTAL'`**: Typically the first call.
        *   Determines the maximum memory (`IWANT`) needed across all possible subsequent actions.
        *   Reads the `&MEMRY` namelist (from unit 3) which can provide a user-defined `IWANT`, a `NOREL` flag (if true, memory is allocated once and not released until `RELSE`), and a `ZPRT` flag.
        *   If `NOREL` is true (or for non-INTEL systems generally when `NOREL` isn't explicitly false via input), this call effectively allocates the main memory pool `X` for the entire run.
        *   If `NOREL` is false (and not INTEL), it prints the evaluated `IWANT` but doesn't allocate `X` yet.
    *   **`'IJKLH'`**: Calculates memory for reading/Hamiltonian construction step.
        *   Non-INTEL: `LOCM(1-5)` point to segments for `NI4`, `NI3`, `NI2`, `NR4` integrals, followed by other work arrays.
        *   INTEL: `LOCM(1-5)` are effectively placeholders as `ijkl*` arrays are allocated separately if `doit` is true. `LOCM(6-9)` point to other work arrays.
    *   **`'DAVID'`**: Calculates memory for the Davidson diagonalization algorithm. Needs space for CI vectors (`ndb = maxm*ncf`) and auxiliary arrays (`ndab`).
    *   **`'ATOML'`**: Calculates memory for atomic L^2 calculations (`id13*id1`).
    *   **`'DIABA'`**: Calculates memory for diabatization (`nrefmo*id13`, `nsab*nsab`, etc.).
    *   **`'PREPR'`**: Calculates memory for perturbation theory preparation.
    *   **`'MKF04'`**: Calculates memory for writing results to `FILE04`.
    *   **`'IJKLP'`**: Calculates memory for integral reading in the perturbation step (similar to `IJKLH`).
*   **Memory Allocation Logic:**
    *   **Non-INTEL version (or if `NOREL` is true):**
        *   If `ACTION='TOTAL'`, `MEMGET(IWANT)` is called to obtain a large block of memory; `MEMSIZ` is set.
        *   For subsequent actions, it checks if `IWANT <= MEMSIZ`.
    *   **INTEL version (and `NOREL` is false by default unless overridden):**
        *   For each action (except `TOTAL` if `NOREL` is false), `allocate(x(iwant))` is performed, and `MEMSIZ` is updated. This means memory for `X` can be resized for different stages if `NOREL` is false.
*   **Offset Calculation (`LOFFS`):** After memory for `X` is secured, `LOFFS` is calculated. For non-INTEL, this involves `laddrs(x)` (a system-dependent function to get the address of `X`) and `LOCMEM` to find the difference, ensuring `X(LOFFS)` is the actual start of the usable pool. For INTEL, `LOFFS` is simply 1. The `LOCM` array entries are then adjusted by this `LOFFS`.

---

### `MEMDIG`

*   **Signature:** `SUBROUTINE MEMDIG`
*   **Arguments:** None.
*   **Purpose:** A specialized memory allocation routine, likely for programs named "CIPDIAGR" or "CIPNESBET" (as suggested by comments in `MEMCIP` if an unknown action is passed).
*   **Functionality:**
    1.  Calculates `IWANT` based *only* on the integral storage requirements (`NI4`, `NI3`, `NI2`, `NR4` from `COMMON /INT/`).
    2.  Allocates memory for `X` (either via `MEMGET` or `allocate(x)`) similar to `MEMCIP`.
    3.  Sets `MEMSIZ` and calculates `LOFFS`.
    *   **Note (INTEL version):** Contains a `STOP` message "You shouln't be calling memdig" under an `#ifdef NES` block if specific integral arrays are in `/BIG/`, but allows allocation of `x` under an `#ifdef CIP` block. This suggests it's an older or specialized entry point.

---

### `MEMROS`

*   **Signature:** `SUBROUTINE MEMROS`
*   **Arguments:** None.
*   **Purpose:** Another specialized memory allocation routine, likely for a program named "ROSPI".
*   **Functionality:** Identical to `MEMDIG`; calculates `IWANT` based only on integral storage needs and allocates memory for `X`.

---

### `NWRD`

*   **Signature:** `FUNCTION NWRD(NBYTE)`
*   **Arguments:**
    *   `NBYTE`: `INTEGER, INTENT(IN)` - The number of bytes.
*   **Return Type:** `INTEGER`
*   **Purpose:** Converts a size in bytes to a size in double precision words. It calculates `(NBYTE+7)/8`, effectively rounding up to the nearest 8-byte word.

## Hardware/Compiler Specifics

*   The code contains `#ifndef INTEL ... #else ... #endif` blocks, indicating different memory management strategies:
    *   **Non-INTEL:** Relies on a static `COMMON /BIG/ X(1)` and a non-standard `MEMGET` function to obtain memory. Address calculations use `laddrs` (commented out but indicative of system calls).
    *   **INTEL:** Uses Fortran pointers for `COMMON /BIG/ x` and directly allocates/deallocates `x` and potentially other arrays like `ijkl2`, `ijkl3`, etc.
*   Comments like `*DEC`, `*AIX`, `*HPUX`, `*LINUX` point to historical system-dependent ways of obtaining memory addresses.
```
