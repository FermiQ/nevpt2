```markdown
# `input_reader.F90` Module Documentation

## Module Name

`input_reader`

## Purpose

This Fortran module provides a collection of routines designed to facilitate the parsing of keyword-driven input files. It offers functionalities for:

*   Managing the input file unit.
*   String manipulations (case conversion, substring checking).
*   Matching keywords read from an input file against a list of expected keywords.
*   Reading various data types (character, integer, real, integer vectors) associated with recognized keywords.
*   Error handling for unrecognized keywords or malformed input data.

The general workflow implied is:
1.  Set the input file unit using `set_file_unit`.
2.  For each section of the input file:
    a.  Call `reset_available_kw_list`.
    b.  Read a line or potential keyword from the input file.
    c.  Use a series of `if (kw_matches(line_keyword, EXPECTED_KW_1)) then ... else if (kw_matches(line_keyword, EXPECTED_KW_2)) then ...`
    d.  Inside the `if` block, use the `kw_read` interface (e.g., `call kw_read(EXPECTED_KW_X, value)`) to read the data.
    e.  After checking all expected keywords for that line/section, call `check_whether_kw_found` to ensure the `line_keyword` was valid.

## Public Parameters

---

### `kw_length`

*   **Type:** `INTEGER, PARAMETER`
*   **Value:** `7`
*   **Purpose:** Defines the standard fixed length for all keywords used in the input file. Keywords shorter than this are likely expected to be padded with spaces to this length when comparing.

## Public Subroutines and Functions

---

### `set_file_unit`

*   **Signature:** `subroutine set_file_unit(u)`
*   **Arguments:**
    *   `u`: `INTEGER, INTENT(IN)` - The Fortran logical unit number connected to the input file.
*   **Purpose:** Sets the internal module variable that stores the file unit to be used for reading keywords and data.

---

### `get_file_unit`

*   **Signature:** `integer function get_file_unit()`
*   **Arguments:** None.
*   **Purpose:** Returns the currently set Fortran logical unit number for input.

---

### `lowercase`

*   **Signature:** `function lowercase(s) result(lowercase_s)`
*   **Arguments:**
    *   `s`: `CHARACTER(*), INTENT(IN)` - The input string.
*   **Return Type:** `CHARACTER(LEN(s))`
*   **Purpose:** Converts all uppercase English alphabet characters in the input string `s` to lowercase. Other characters remain unchanged.

---

### `uppercase`

*   **Signature:** `function uppercase(s) result(uppercase_s)`
*   **Arguments:**
    *   `s`: `CHARACTER(*), INTENT(IN)` - The input string.
*   **Return Type:** `CHARACTER(LEN(s))`
*   **Purpose:** Converts all lowercase English alphabet characters in the input string `s` to uppercase. Other characters remain unchanged.

---

### `word_contains`

*   **Signature:** `logical function word_contains(word, substring)`
*   **Arguments:**
    *   `word`: `CHARACTER(*), INTENT(IN)` - The string to be searched.
    *   `substring`: `CHARACTER(*), INTENT(IN)` - The substring to search for.
*   **Purpose:** Returns `.TRUE.` if `substring` is found within `word`, and `.FALSE.` otherwise. This is a simple wrapper around the intrinsic `INDEX` function.

---

### `reset_available_kw_list`

*   **Signature:** `subroutine reset_available_kw_list()`
*   **Arguments:** None.
*   **Purpose:** Resets the internal list of "available" keywords that `kw_matches` populates for error reporting. It sets the count of available keywords (`nr_available_kw`) to zero and the `kw_found` flag to `.FALSE.`. This should be called before parsing a new section or block of keywords in the input file.

---

### `check_whether_kw_found`

*   **Signature:** `subroutine check_whether_kw_found(kw_input, kw_section)`
*   **Arguments:**
    *   `kw_input`: `CHARACTER(kw_length), INTENT(IN)` - The keyword string that was actually read or processed from the input.
    *   `kw_section`: `CHARACTER(kw_length), INTENT(IN)` - A string identifying the current section or context of the input file being parsed.
*   **Purpose:** Checks the internal `kw_found` flag (which is set to `.TRUE.` by a successful `kw_matches` call). If `kw_found` is `.FALSE.`, it means the `kw_input` did not match any of the expected keywords for the `kw_section`. In this case, it prints an error message listing all the valid keywords that were registered (via `kw_matches`) for that section and then stops program execution.

---

### `kw_matches`

*   **Signature:** `logical function kw_matches(kw_input, kw_option)`
*   **Arguments:**
    *   `kw_input`: `CHARACTER(kw_length), INTENT(IN)` - The keyword string read from the current input line (or a substring thereof).
    *   `kw_option`: `CHARACTER(kw_length), INTENT(IN)` - An expected keyword to compare against.
*   **Purpose:**
    1.  Compares `lowercase(kw_input)` with `lowercase(kw_option)`.
    2.  If they match:
        *   Sets the function result to `.TRUE.`.
        *   Sets the internal module flag `kw_found` to `.TRUE.`.
    3.  If they do not match:
        *   Sets the function result to `.FALSE.`.
        *   Increments the internal counter `nr_available_kw`.
        *   Stores `kw_option` in the `available_kw_list(nr_available_kw)` array (used by `check_whether_kw_found` for error messages).
*   **Usage:** This function is typically called in a sequence (e.g., `if/elseif` chain) to test an input keyword against all valid keywords for the current parsing context.

---

### `kw_read` (Generic Interface)

This generic interface allows reading data of different types associated with a previously matched keyword. The actual read operation is performed by one of the specific `kw_read_...` subroutines based on the type of the output argument(s).

*   **Specific Subroutines implementing `kw_read`:**

    *   **`kw_read_c(kw_input, c)`**
        *   `kw_input`: `CHARACTER(kw_length), INTENT(IN)` - The matched keyword (for error reporting).
        *   `c`: `CHARACTER(*), INTENT(OUT)` - Variable to store the read character string.
        *   **Purpose:** Reads a character string from the current line of `file_unit`.

    *   **`kw_read_i1(kw_input, i)`**
        *   `kw_input`: `CHARACTER(kw_length), INTENT(IN)`.
        *   `i`: `INTEGER, INTENT(OUT)` - Variable to store the read integer.
        *   **Purpose:** Reads a single integer from `file_unit`.

    *   **`kw_read_i3(kw_input, i1, i2, i3)`**
        *   `kw_input`: `CHARACTER(kw_length), INTENT(IN)`.
        *   `i1, i2, i3`: `INTEGER, INTENT(OUT)` - Variables to store three read integers.
        *   **Purpose:** Reads three integers from `file_unit`.

    *   **`kw_read_r1(kw_input, r)`**
        *   `kw_input`: `CHARACTER(kw_length), INTENT(IN)`.
        *   `r`: `REAL(8), INTENT(OUT)` - Variable to store the read double precision real.
        *   **Purpose:** Reads a single double precision real from `file_unit`.

    *   **`kw_read_r2(kw_input, r1, r2)`**
        *   `kw_input`: `CHARACTER(kw_length), INTENT(IN)`.
        *   `r1, r2`: `REAL(8), INTENT(OUT)` - Variables to store two read double precision reals.
        *   **Purpose:** Reads two double precision reals from `file_unit`.

    *   **`kw_read_r3(kw_input, r1, r2, r3)`**
        *   `kw_input`: `CHARACTER(kw_length), INTENT(IN)`.
        *   `r1, r2, r3`: `REAL(8), INTENT(OUT)` - Variables to store three read double precision reals.
        *   **Purpose:** Reads three double precision reals from `file_unit`.

    *   **`kw_read_r4(kw_input, r1, r2, r3, r4)`**
        *   `kw_input`: `CHARACTER(kw_length), INTENT(IN)`.
        *   `r1, r2, r3, r4`: `REAL(8), INTENT(OUT)` - Variables to store four read double precision reals.
        *   **Purpose:** Reads four double precision reals from `file_unit`.

    *   **`kw_read_ivec(kw_input, v, k)`**
        *   `kw_input`: `CHARACTER(kw_length), INTENT(IN)`.
        *   `v(:)`: `INTEGER, INTENT(OUT)` - Array to store the read integers.
        *   `k`: `INTEGER, INTENT(IN)` - The number of integers to read into array `v`.
        *   **Purpose:** Reads `k` integers from `file_unit` into the integer array `v`.
*   **Error Handling:** All `kw_read_...` subroutines call the private `kw_read_error` subroutine if a Fortran `READ` statement encounters an error (e.g., end-of-file, format mismatch). `kw_read_error` prints details about the error and stops the program.

---
```
