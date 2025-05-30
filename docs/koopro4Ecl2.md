```markdown
# `koopro4Ecl2.F` Program Documentation

## Program Overview

`koopro4Ecl2.F` is a FORTRAN main program designed to calculate various n-particle density matrices (1-RDM, 2-RDM, 3-RDM, and optionally 4-RDM) and different types of Koopmans-like matrices. These matrices are crucial intermediates for advanced quantum chemistry calculations, particularly for n-electron valence state perturbation theory (NEVPT2).

The program takes data from a preceding Configuration Interaction (CI) or CASSCF calculation (energies, CI vectors, determinant definitions) and molecular integrals as input. It then computes the specified density and Koopmans matrices and writes them to a binary output file (typically `FILE31`) for use in subsequent perturbation theory steps (e.g., by a program like `dypc2.F`).

The name "koopro" likely stands for "Koopmans Procedure," while "4Ecl2" might refer to a specific context or variant of the calculation it performs, possibly related to 4-electron systems or a particular class of terms (though the code itself is general for `nact` active electrons).

## Input

### NAMELIST `&LEGGI` (Read from Unit 5)

| Parameter  | Type    | Default   | Description                                                                                                                               |
| :--------- | :------ | :-------- | :---------------------------------------------------------------------------------------------------------------------------------------- |
| `NCORE`    | INTEGER | `0`       | Number of core (doubly occupied, inactive) orbitals.                                                                                      |
| `NACT`     | INTEGER | `0`       | Number of active orbitals.                                                                                                                |
| `THR`      | REAL\*8 | `1.0D-10` | Threshold for CI coefficients; determinants with coefficients below this might be neglected in RDM calculations.                          |
| `NSPIN`    | INTEGER | `1`       | Spin multiplicity of the state (2S+1).                                                                                                    |
| `FILE04`   | CHARACTER\*80 | `' '`   | Path to the unformatted file containing CI vectors, energies, and determinant information from a previous calculation. (Mandatory)      |
| `FILE50`   | CHARACTER\*80 | `' '`   | Path to the unformatted file containing two-electron integrals. (Mandatory)                                                               |
| `FILE26`   | CHARACTER\*80 | `' '`   | Path to the unformatted file containing one-electron integrals (used if `ZTOUL=T`). (Mandatory if `ZTOUL=T`)                             |
| `ZTOUL`    | LOGICAL | `.FALSE.` | If `.TRUE.`, use Toulouse integral format (implies reading one-electron integrals from `FILE26`).                                           |
| `FILE25`   | CHARACTER\*80 | `' '`   | Path to the unformatted file containing orbital symmetry information (read by `REIJKL`). (Mandatory)                                       |
| `FILE31`   | CHARACTER\*80 | `' '`   | Path for the primary unformatted output file where computed density and Koopmans matrices will be written. (Mandatory output destination) |
| `ZRO4`     | LOGICAL | `.FALSE.` | If `.TRUE.`, compute and write the 4-particle density matrix (`RO4`). Otherwise, it's skipped.                                              |
| `ZORDER`   | LOGICAL | `.TRUE.`  | If `.TRUE.`, assumes determinants are ordered in a specific way (e.g., by CSF groups) for `icomp` usage. If `.FALSE.`, each determinant is treated individually. |
| `ZVERBOSE` | LOGICAL | `.FALSE.` | If `.TRUE.`, enables more detailed print output during execution.                                                                           |

### File Units

*   **Unit 4 (Input):** Specified by `FILE04`. Contains reference wavefunction data (CI vectors, energies, determinant definitions, orbital energies, integral factor) from a preceding CI or CASSCF calculation.
*   **Unit 5 (Input):** Standard input, used to read the `&LEGGI` namelist.
*   **Unit 6 (Output):** Standard output, used for program logs, printing parameters, and progress information.
*   **Unit 25 (Input):** Specified by `FILE25`. Contains orbital symmetry information, number of orbitals, etc., typically read by an internal or linked `REIJKL` subroutine.
*   **Unit 26 (Input, Optional):** Specified by `FILE26`. Used to read one-electron integrals if `ZTOUL=.TRUE.`.
*   **Unit 31 (Output):** Specified by `FILE31`. The primary binary output file where all computed density matrices (1-RDM, 2-RDM, 3-RDM, optionally 4-RDM) and various Koopmans matrices are written. This file serves as input to subsequent NEVPT2 programs like `dypc2.F`.
*   **Unit 50 (Input):** Specified by `FILE50`. Contains the two-electron molecular integrals. Handled with `filesplit` for potentially large files.

## Output

*   **Primary Output:** Unformatted binary data written to the file specified by `FILE31` in the `&LEGGI` namelist. This file includes:
    *   1-particle density matrix (`DAL`).
    *   1-electron Koopmans matrices for Electron Affinity (`COOPEAA`) and Ionization Potential (`COOPPRE`).
    *   2-hole density matrix (`RO2T` stored in `KOOPAA`).
    *   2-particle density matrix (`TAA`).
    *   2-electron Koopmans matrices for EA (`KOOPEAA` from `koop2E`) and IP (`KOOPAA` from `koop2E`).
    *   Generalized Koopmans matrices for semi-internal terms: `gk0pa` (from `KOOPAA`), `gk0pd` (from `KOOPBB`), `gk0pf` (from `KOOPAB`).
    *   Additional matrices: `AMAT`, `ATMAT`, `BMAT`, `BTMAT`, `CMAT`, `CTMAT`, `DMAT`, `DTMAT`.
    *   Optionally, the 4-particle density matrix (`RO4`) if `ZRO4=.TRUE.`.
*   **Standard Output (Unit 6):** Program execution log, echo of input parameters, timing information, and diagnostic messages.

## Core Functionality and Workflow

1.  **Initialization:**
    *   Sets default values for parameters in `&LEGGI`.
    *   Reads the `&LEGGI` namelist from Unit 5.
    *   Opens required input and output files.
2.  **Read Reference Data:**
    *   Reads CI/CASSCF wavefunction information (determinant structure, CI coefficients `C`, energies, orbital data, integral factor) from `FILE04`.
    *   Determines `NCORE` and `NACT` if not set in the namelist, based on the input wavefunction.
    *   Calculates `NELE` (number of active electrons).
3.  **Integral Processing:**
    *   Calls a local or linked `readint` subroutine. This routine, in turn, likely calls an older-style `REIJKL` and `ITIJKL` (or `ITIJKLt`) to process one- and two-electron integrals from `FILE25`, `FILE50` (and `FILE26` if `ZTOUL`).
    *   Prepares active space two-electron integrals (`ATWO`) and effective one-electron integrals (`F`, `AJ`).
4.  **Density Matrix Calculation:**
    *   The program computes N-particle density matrices for the active space based on the CI vector `C` and determinant definitions.
    *   If `NELE >= 4`: Calls `rofour` to compute the 4-RDM (`RO4`), then `bro3` (RO4->DAAA), `bro2` (DAAA->TAA), `bro1` (TAA->DAL).
    *   If `NELE = 3`: Calls `ro3` (DAAA), then `bro2` (DAAA->TAA), `bro1` (TAA->DAL).
    *   If `NELE = 2`: Calls `ro2` (TAA), then `bro1` (TAA->DAL).
    *   If `NELE = 1`: Calls `ro1` (DAL).
    *   These routines (`rofour`, `ro3`, etc.) use helper subroutines like `esclass`, `giveocc`, `giveocc2`, `jdifgen`, and various permutation/ordering utilities to handle determinant comparisons and contributions.
5.  **Koopmans Matrix Calculation:**
    *   Calls `koopE` to compute 1-electron IP/EA type Koopmans matrices.
    *   Calls `koop2E` to compute 2-electron IP/EA type Koopmans matrices.
    *   Calls `koopman0pE` to compute generalized Koopmans matrices (`gk0pa`, `gk0pd`, `gk0pf`) needed for semi-internal terms in NEVPT2.
    *   Calls `bamat`, `bbmat`, `bcmat`, `bdmat` to compute other related matrices (`AMAT`, `BMAT`, etc.).
6.  **Write Output:**
    *   Uses a series of `wr1`, `wr2`, `wr3`, `wr4`, `wr5`, (and `wr6` if `ZRO4=T`) subroutines to write all computed matrices to the binary file specified by `FILE31`.

## Key Variables and Arrays

*   `NCORE`, `NACT`: Number of core and active orbitals.
*   `DAL`, `TAA`, `DAAA`, `RO4`: Arrays storing the 1-, 2-, 3-, and 4-particle density matrices of the active space, respectively.
*   `COOPPRE`, `COOPEAA`: 1-electron Koopmans matrices (IP and EA types).
*   `KOOPAA`, `KOOPEAA` (from `koop2E`): 2-electron Koopmans matrices (IP and EA types).
*   `KOOPAA`, `KOOPBB`, `KOOPAB` (from `koopman0pE`): Store `gk0pa`, `gk0pd`, `gk0pf` matrices for NEVPT2.
*   `AMAT`, `BMAT`, `CMAT`, `DMAT`, `ATMAT`, `BTMAT`, `CTMAT`, `DTMAT`: Additional computed matrices, likely intermediates or components for NEVPT2.
*   `C`: Array of CI coefficients from the reference calculation.
*   `ATWO`: Array of two-electron integrals in the active space.
*   `F`, `AJ`: Arrays for effective one-electron integrals.

## Dependencies and Called Routines

`koopro4Ecl2.F` is a driver program and relies on:

*   Input data files for CI information and integrals.
*   A large number of local or linked Fortran subroutines for its computations. These include:
    *   Density matrix calculation routines (`rofour`, `ro3`, `ro2`, `ro1`, `bro3`, `bro2`, `bro1`).
    *   Koopmans matrix calculation routines (`koopE`, `koop2E`, `koopman0pE`, `bamat`, `bbmat`, `bcmat`, `bdmat`).
    *   Integral processing routines (a local `readint`, and likely older versions of `REIJKL`, `ITIJKL`, `AI`, `PICK*` functions if not using a module like `ijkl_utils`).
    *   Determinant and occupation utilities (`esclass`, `giveocc`, `giveocc2`, `jdifgen`, `ord*`, `permut*`).
    *   File writing utilities (`wr1` through `wr6`).
*   Common blocks like `/CIP/`, `/BUPA/`, `/ACTSPACE/`, `/LNDC/`, `/INT/`, `/TOUL/` are used for passing data between these routines if they are not using modules.
```
