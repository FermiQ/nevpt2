```markdown
# `input_reader_sections.F90` Subroutines Documentation

## File Overview

The file `input_reader_sections.F90` contains a set of Fortran subroutines designed to parse specific named sections within a larger input file for a quantum chemistry program, likely NEVPT2. These subroutines work in conjunction with the `input_reader` module (which provides keyword matching and data reading tools) and the `nevpt2_cfg` module (which stores the configuration parameters read from the input).

The primary entry point for section parsing is `read_input_sections`, which then dispatches to section-specific parsing routines like `read_input_koopro`, `read_input_qdnevpt`, and `read_input_model`.

## Dependencies

*   **`input_reader` module:** Used extensively for core parsing functionalities:
    *   `kw_length`: Parameter defining the length of keywords.
    *   `reset_available_kw_list()`: To initialize keyword tracking for a section.
    *   `kw_matches()`: To check if an input line matches an expected keyword.
    *   `kw_read()`: Generic interface to read data associated with a keyword.
    *   `check_whether_kw_found()`: To validate that an input keyword was recognized within the section.
*   **`nevpt2_cfg` module:** This module is used to store the configuration parameters that are read by these subroutines. Variables like `thr`, `nspin`, `nr_states`, `file04`, `do_dmrg_pt`, etc., are assumed to be defined in `nevpt2_cfg`.

## Subroutines

---

### `read_input_sections`

*   **Signature:** `subroutine read_input_sections(word, kw_section)`
*   **Arguments:**
    *   `word`: `CHARACTER(kw_length), INTENT(IN)` - The keyword identifying the input section (e.g., "*KOOPRO", "*QDNEVP").
    *   `kw_section`: `CHARACTER(kw_length), INTENT(IN)` - The same as `word`, passed down to specific parsing routines for context in error messages.
*   **Purpose:** Acts as a dispatcher to call the appropriate section-specific parsing subroutine based on the value of `kw_section`.
*   **Recognized Sections:**
    *   `*KOOPRO`: Calls `read_input_koopro`.
    *   `*QDNEVP`: Calls `read_input_qdnevpt`.
    *   `*MODEL`: Calls `read_input_model`.
    *   If the section is not recognized, it prints an error message and stops execution.

---

### `read_input_koopro`

*   **Signature:** `subroutine read_input_koopro(word, kw_section)`
*   **Arguments:**
    *   `word`: `CHARACTER(kw_length), INTENT(IN)` - The keyword read from a line *within* the `*KOOPRO` section.
    *   `kw_section`: `CHARACTER(kw_length), INTENT(IN)` - The name of the current section (`*KOOPRO`), used for error reporting.
*   **Purpose:** Parses keywords and associated values within the `*KOOPRO` input section. It uses routines from the `input_reader` module. Configuration values are stored in variables from the `nevpt2_cfg` module.
*   **Handled Keywords:**
    *   `.THRESH`: Reads a real value into `thr`.
    *   `.SPIN  `: Reads an integer value into `nspin`.
    *   `.STATES`: Reads an integer value into `nr_states`.
    *   `.NACTEL`: Reads an integer value into `nr_active_electrons`.
    *   `.NCORE `: Reads an integer value into `ncore` (noted as for non-MOLCAS driven versions).
    *   `.NACTOR`: Reads an integer value into `nact` (noted as for non-MOLCAS driven versions).
    *   `.FILE04`: Reads a character string into `file04`.
    *   `.FILE50`: Reads a character string into `file50`.
    *   `.NO ZOR`: Sets the logical variable `zorder` to `.FALSE.`.
*   **Error Handling:** Calls `check_whether_kw_found` at the end to ensure the `word` matched one of the known keywords in this section.

---

### `read_input_qdnevpt`

*   **Signature:** `subroutine read_input_qdnevpt(word, kw_section)`
*   **Arguments:**
    *   `word`: `CHARACTER(kw_length), INTENT(IN)` - The keyword read from a line *within* the `*QDNEVP` section.
    *   `kw_section`: `CHARACTER(kw_length), INTENT(IN)` - The name of the current section (`*QDNEVP`).
*   **Purpose:** Parses keywords and associated values within the `*QDNEVP` input section. Values are stored in variables from the `nevpt2_cfg` module.
*   **Handled Keywords:**
    *   `.LINDEP`: Reads a real value into `thr_lindep`.
    *   `.STATES`: Reads an integer value into `nr_states`.
    *   `.FROZEN`: Reads an integer into `nr_frozen_orb`, then allocates the `igelo` array (from `nevpt2_cfg`) to this size, and reads `nr_frozen_orb` integers into `igelo`.
    *   `.FILE04`: Reads a character string into `file04`.
    *   `.NATORB`: Sets logical flags `compute_nooccn` and `compute_rho1st` to `.TRUE.`.
    *   `.NO PC `: Sets logical flag `no_pc` to `.TRUE.`.
*   **Error Handling:** Calls `check_whether_kw_found` at the end.

---

### `read_input_model`

*   **Signature:** `subroutine read_input_model(word, kw_section)`
*   **Arguments:**
    *   `word`: `CHARACTER(kw_length), INTENT(IN)` - The keyword read from a line *within* the `*MODEL` section.
    *   `kw_section`: `CHARACTER(kw_length), INTENT(IN)` - The name of the current section (`*MODEL`).
*   **Purpose:** Parses keywords and associated values within the `*MODEL` input section, which typically defines model Hamiltonian choices or computational strategies. Values are stored in variables from the `nevpt2_cfg` module.
*   **Handled Keywords:**
    *   `.DMRGPT`: Sets logical flag `do_dmrg_pt` to `.TRUE.`.
    *   `.CHOLES`: Sets logical flag `do_cholesky` to `.TRUE.`.
    *   `.NO4RDM`: Sets logical flag `no_4rdm_terms` to `.TRUE.`.
*   **Error Handling:** Calls `check_whether_kw_found` at the end.

---
```
