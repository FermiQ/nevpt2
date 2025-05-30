```markdown
# `cippiS2.F` Documentation

## Program Name

CIPSI (Configuration Interaction by Perturbation of a multiconfigurational zeroth-order wavefunction Selected by Iterative process) / CIPPI (modified version, Pisa) / cippiS2

## Version Information

*   Original CIPSI: Toulouse
*   CIPPI modification: Pisa, January 1993
*   Further modifications: Ferrara 2000
*   Compilation note: "Version compiled with intel ifc compiler"

## Description

`cippiS2.F` is a FORTRAN program for performing Configuration Interaction (CI) calculations, primarily focused on:

1.  **Variational Calculation:** Diagonalization of the Hamiltonian matrix within a user-defined space of determinants (referred to as space S), which are S^2 eigenstates.
2.  **Perturbative Calculation:** Second-order perturbation theory (Epstein-Nesbet or Møller-Plesset type) to account for excitations (single and double by default, but up to dodecuple excitations can be treated) outside the variational space S.

The program is designed for quantum chemistry calculations to determine electronic state energies and wavefunctions.

## Core Functionality

*   **Variational CI:** Solves the CI problem in a selected determinant space (S).
*   **Perturbation Theory:** Calculates second-order energy corrections using either Epstein-Nesbet (EN) or Møller-Plesset (MP) baricentric approaches.
*   **S^2 Eigenstates:** Works with configurations that are eigenstates of the total spin squared (S^2) operator.
*   **Excitation Levels:** Can handle excitations from single up to dodecuple (12-fold) from the reference determinants in space S.
*   **Diabatization:** Includes options for diabatization of adiabatic states.
*   **Spin and Angular Momentum Properties:** Can calculate matrix elements for S^2, L^2, and Lz^2.

## Input Files

*   **Unit 3 (`CIPPI.FILE03` or user-defined):** A copy of the determinant definition cards read from standard input or `FILEDET`.
*   **Unit 4 (`CIPPI.FILE04` or user-defined):**
    *   Input (if `ZDET=T`): Determinant definitions, CI vectors from a previous run.
    *   Output: Variational results (energies, CI vectors), effective Hamiltonian for QDPT.
*   **Unit 5 (Standard Input or user-defined `FILE05`):**
    *   General input data (title, NAMELISTs).
    *   Determinant definitions for space S (if `ZDET=F`).
*   **`FILEDET` (Optional, specified in `&FILES`):** Formatted file to read determinant definitions instead of Unit 5.
*   **Unit 11 (user-defined `FILE11`):** Unformatted file, likely from a previous run, potentially containing MOs or other data for restart/projection.
*   **Unit 14 (Optional, `PROJF` in `&DIAGO` or `FILE14` in `&FILES`):**
    *   If `ZVEC=T` and `PROJF` is set, reads trial vectors for Davidson diagonalization.
    *   If `ZCOST=T`, reads `.Info` file from a COST program.
*   **Unit 20 (`CIPPI.FILE20` or user-defined):** Input for CI matrix if `ZHMAT=F`.
*   **Unit 25 (`IJKL.FILE25` or user-defined):**
    *   Input: Contains symmetry information, number of orbitals (`NSYM`, `NORB`), number of occupied orbitals (`NOC`), degeneracies, orbital symmetries (`ITSYM`), SCF energy (`ESCF`), nuclear repulsion energy (`ENUC`), and orbital occupation numbers (`TOCOM`).
    *   Output: Updated `ESCF`, `ENUC`, and permutation array `NPERM` (if `ZTOUL=T`).
*   **Unit 26 (user-defined `FILE26`):**
    *   Input (if `ZTOUL=T`): `.mono` file from MOLCSD (one-electron integrals).
    *   Output: Contains one-electron integrals.
*   **Unit 28 (Optional, `FILE28` in `&FILES`):** Unformatted `.Mono` file from a COST program (if `ZCOST=T`).
*   **Unit 50 (`IJKL.FILE50` or user-defined):** Input for two-electron integrals.

## Output Files

*   **Unit 3 (`CIPPI.FILE03` or user-defined):** Copy of determinant input cards.
*   **Unit 4 (`CIPPI.FILE04` or user-defined):**
    *   Variational CI vectors and energies.
    *   Effective Hamiltonian for QDPT.
*   **Unit 6 (Standard Output):** Program log, titles, input parameters, energies, Davidson convergence details, perturbation summaries, S^2/L^2 values, final energies, etc.
*   **Unit 7 (`CIPPI.FILE07` or user-defined):**
    *   Data for subsequent selection/analysis programs (e.g., CIPSELX). Includes norms of CI vectors, energies, perturbative corrections.
    *   Can be formatted or binary (controlled by `ZBIN`).
*   **Unit 9 (`CIPPI.FILE09` or user-defined):** Scratch file for unformatted data.
*   **Unit 29 (Optional, `FILE29` in `&FILES` if `ZWCONF=T`):** CSF coefficients (unformatted).

## NAMELIST Parameters

### `&FILES`

| Parameter | Type    | Default                                   | Description                                                                                                                               |
| :-------- | :------ | :---------------------------------------- | :---------------------------------------------------------------------------------------------------------------------------------------- |
| `FILE03`  | CHARACTER | Derived from `FILE04` or `CIPPI.FILE03` | Output file for determinant definitions.                                                                                                  |
| `FILE04`  | CHARACTER | `CIPPI.FILE04`                          | Main output/input for CI vectors and energies.                                                                                            |
| `FILE07`  | CHARACTER | `' '` (not opened)                      | Output file for perturbation data (for CIPSELX).                                                                                          |
| `FILE09`  | CHARACTER | `CIPPI.FILE09`                          | Unformatted scratch file.                                                                                                                 |
| `FILE11`  | CHARACTER | `' '` (not opened)                      | Input file, possibly for MOs or restart.                                                                                                  |
| `FILE12`  | CHARACTER | `' '`                                   | User-defined. Referenced in context of `MODISK` for reference MOs.                                                                        |
| `FILE13`  | CHARACTER | `DIAGO.RESTART`                         | Restart file for Davidson diagonalization.                                                                                                |
| `ZCOST`   | LOGICAL | `.FALSE.`                                 | If `.TRUE.`, read `FILE14` (`.Info` from COST) and `FILE28` (`.Mono` from COST).                                                          |
| `FILE14`  | CHARACTER | `' '`                                   | Input for `.Info` from COST (if `ZCOST=T`). Also used as `PROJF` in `&DIAGO`.                                                             |
| `FILE28`  | CHARACTER | `' '`                                   | Input for `.Mono` from COST (if `ZCOST=T`).                                                                                               |
| `FILE20`  | CHARACTER | `CIPPI.FILE20`                          | Input for CI matrix if `ZHMAT=F`.                                                                                                         |
| `FILE25`  | CHARACTER | `IJKL.FILE25`                           | Input/Output for orbital information and SCF energy.                                                                                      |
| `FILE26`  | CHARACTER | `' '`                                   | Input for one-electron integrals if `ZTOUL=T` (MOLCSD format).                                                                              |
| `ZWCONF`  | LOGICAL | `.FALSE.`                                 | If `.TRUE.`, CSF coefficients are written to `FILE29`.                                                                                    |
| `FILE29`  | CHARACTER | `' '`                                   | Output for CSF coefficients (if `ZWCONF=T`).                                                                                              |
| `FILE50`  | CHARACTER | `IJKL.FILE50`                           | Input for two-electron integrals.                                                                                                         |
| `FILE05`  | CHARACTER | `' '`                                   | Input file for main program data (namelists, determinants if `ZDET=F`). Defaults to standard input if not specified.                       |
| `FILEDET` | CHARACTER | `' '`                                   | Formatted input file for determinant definitions (alternative to standard input).                                                         |
| `ZBIN`    | LOGICAL | `.TRUE.`                                  | If `.TRUE.`, `FILE07` is written in binary form. If `.FALSE.`, `FILE07` is formatted. (Default in comment says `.F.`, code init says `.T.`) |

### `&ICINP`

| Parameter  | Type         | Default                             | Description                                                                                                                                                                                                                                                                                                                                                        |
| :--------- | :----------- | :---------------------------------- | :----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `ZDET`     | LOGICAL      | `.FALSE.`                           | If `.TRUE.`, determinants are read from `FILE04`. If `.FALSE.`, from `FILE05` (or `FILEDET`).                                                                                                                                                                                                                                                                          |
| `ISZ`      | INTEGER      | `0`                                 | Z component of total spin S (S_z).                                                                                                                                                                                                                                                                                                                                 |
| `ZION`     | LOGICAL      | `.FALSE.`                           | If `.FALSE.`, even number of electrons. If `.TRUE.`, odd number of electrons; a fictitious non-interacting orbital (NORB+1) with spin +1/2 is added. Requires `ISZ=1`.                                                                                                                                                                                              |
| `METAT`    | INTEGER      | `1`                                 | Number of states treated in perturbation.                                                                                                                                                                                                                                                                                                                          |
| `NROT`     | INTEGER      | `METAT`                             | Number of eigenvectors determined in space S. `NROT >= METAT`. See also `IETATS` in `&DIAGO` and `NREF` in `&DIABA`.                                                                                                                                                                                                                                                   |
| `ZPERTU`   | LOGICAL      | `.TRUE.`                            | If `.TRUE.`, perform perturbative calculation. If `.FALSE.`, no perturbation.                                                                                                                                                                                                                                                                                       |
| `ZENB`     | LOGICAL      | `.FALSE.`                           | If `.FALSE.`, Epstein-Nesbet (ENVP) calculation. If `.TRUE.`, Epstein-Nesbet baricentric (ENB) calculation.                                                                                                                                                                                                                                                           |
| `ZMPPUN`   | LOGICAL      | `.FALSE.`                           | If `.TRUE.`, data in `FILE07` are for MP perturbation. If `.FALSE.`, data are for EN perturbation.                                                                                                                                                                                                                                                                      |
| `TEST`     | REAL\*8      | `0.05`                              | Printing threshold for perturbing determinants. Determinant I perturbing state M is printed if `C(M,I) >= TEST * MAX(C0(M,K))`. `C0` are coefficients in S, `C` is perturbative coefficient.                                                                                                                                                                          |
| `TAU`      | REAL\*8      | `TEST`                              | Punching threshold for determinants, similar to `TEST`. `TAU <= TEST`.                                                                                                                                                                                                                                                                                               |
| `ZQD`      | LOGICAL      | `.TRUE.`                            | If `.TRUE.`, quasi-degenerate determinants (where `EMPB(I) <= MAX(EMPB(M)) + DELTA`) are printed and punched.                                                                                                                                                                                                                                                         |
| `DELTA`    | REAL\*8      | `0.10`                              | Energy threshold (in Hartree) for defining quasi-degenerate determinants.                                                                                                                                                                                                                                                                                              |
| `ZBRD`     | LOGICAL      | `.FALSE.`                           | If `.TRUE.`, calculate effective Hamiltonian for QDPT.                                                                                                                                                                                                                                                                                                             |
| `ZPRT`     | LOGICAL      | `.FALSE.`                           | If `.TRUE.`, write intermediate results (J/K integrals, logical tables, etc.).                                                                                                                                                                                                                                                                                       |
| `ZPUN`     | LOGICAL      | `.FALSE.`                           | If `.TRUE.`, selected determinants for printing are also punched to `FILE07`.                                                                                                                                                                                                                                                                                        |
| `ZHMAT`    | LOGICAL      | `.TRUE.`                            | If `.TRUE.`, calculate the CI matrix (subroutine `HMAT`). If `.FALSE.`, read CI matrix from `FILE20`.                                                                                                                                                                                                                                                                  |
| `KGEN`     | INTEGER ARRAY| All S determinants are generators   | Vector of generator determinant numbers (used only if `ZDET=F`).                                                                                                                                                                                                                                                                                                   |
| `ZRAP`     | LOGICAL      | `.FALSE.`                           | If `.TRUE.`, approximate by neglecting interaction between non-generator S determinants and perturbers (faster). If `.FALSE.`, rigorous interaction calculation.                                                                                                                                                                                                    |
| `ZDIAB`    | LOGICAL      | `.FALSE.`                           | If `.TRUE.`, rotate zero-order vectors by maximum overlap with a reference (diabatic) basis. Implies `ZBRD=T`.                                                                                                                                                                                                                                                        |
| `ZSPIN`    | LOGICAL      | `.FALSE.`                           | If `.TRUE.`, calculate S^2 matrix elements.                                                                                                                                                                                                                                                                                                                        |
| `ZSROT`    | LOGICAL      | `.FALSE.`                           | If `.TRUE.`, modify eigenvectors in S to diagonalize S^2. Implies `ZBRD=T`.                                                                                                                                                                                                                                                                                            |
| `ZATOM`    | LOGICAL      | `.FALSE.`                           | If `.TRUE.`, calculate L^2 and Lz^2 matrix elements (for atoms only, see `&ATOMO`).                                                                                                                                                                                                                                                                                |
| `ZDIALZ`   | LOGICAL      | `.FALSE.`                           | If `.TRUE.`, modify eigenvectors in S to diagonalize Lz^2 by blocks (blocks defined by degenerate energy and L^2 subspaces).                                                                                                                                                                                                                                           |
| `TDEGEN`   | REAL\*8      | `1.0D-7`                            | Threshold to define degeneracy of eigenvectors in `DEGEN` and `ATOM` subroutines.                                                                                                                                                                                                                                                                                  |
| `SHIFT(I)` | REAL\*8 ARRAY| `0.0`                               | Energy shifts for MP and EN denominators of perturbing determinants, generating modified perturbative sums. `I=1,NSHIFT`.                                                                                                                                                                                                                                             |
| `NSHIFT`   | INTEGER      | `0`                                 | Number of denominator alterations.                                                                                                                                                                                                                                                                                                                                 |
| `ZCAS`     | LOGICAL      | `.FALSE.`                           | If `.TRUE.`, the zero-order wavefunction is of CAS type (can speed up controls).                                                                                                                                                                                                                                                                                   |
| `ZTHIRD`   | LOGICAL      | `.FALSE.`                           | If `.TRUE.`, print coefficients for subsequent third-order calculation.                                                                                                                                                                                                                                                                                            |
| `ZMP3`     | LOGICAL      | `.FALSE.`                           | If `.TRUE.`, perform Møller-Plesset third-order calculation.                                                                                                                                                                                                                                                                                                       |
| `ZEN3`     | LOGICAL      | `TRUE` (if `ZTHIRD=T`)              | If `.TRUE.`, perform Epstein-Nesbet third-order calculation. `ZEN3` prevails if both `ZMP3` and `ZEN3` are true.                                                                                                                                                                                                                                                     |
| `ZHANDY`   | LOGICAL      | `.TRUE.`                            | If `.TRUE.`, integrals stored as `INTEGER*2`. If `.FALSE.`, as `INTEGER*4`.                                                                                                                                                                                                                                                                                         |
| `FACTOR`   | REAL\*8      | `1.0D6` (if `ZHANDY=T`), `1.0D9` (if `ZHANDY=F`) | Multiplicative factor for integrals to store them as `INTEGER*2` (`IJKL2`). If `ABS(A*FACTOR) > 20000`, integral `A` is stored in `RIJKL` (REAL\*4). Deprecated, use `ZFAC`.                                                                                                                                                                           |
| `ZFAC`     | LOGICAL      | `.FALSE.`                           | If `.TRUE.`, `FACTOR` is calculated automatically based on the maximum integral. If `.FALSE.`, `FACTOR` uses its default or input value.                                                                                                                                                                                                                            |
| `IGEL`     | INTEGER ARRAY| No frozen orbitals                  | Indices of orbitals to be kept inactive (doubly occupied or empty) in the perturbative part.                                                                                                                                                                                                                                                                       |
| `ZDAV`     | LOGICAL      | `.FALSE.`                           | If `.FALSE.`, EN denominators use energy of perturbing determinants. If `.TRUE.`, use Davidson's average procedure (average over determinants with given orbital occupation).                                                                                                                                                                                   |
| `DIR`      | CHARACTER    | `/home/renzo/diago/`                | Directory containing template (T) matrices.                                                                                                                                                                                                                                                                                                                        |
| `ZDH`      | LOGICAL      | `.FALSE.`                           | If `.TRUE.`, all perturbing determinants have orbital `NDHB` "holed" (for core IP calculations).                                                                                                                                                                                                                                                                   |
| `NDHB`     | INTEGER      | `0`                                 | Spatial core orbital ionized (used with `ZDH=T`).                                                                                                                                                                                                                                                                                                                  |
| `ZDDH`     | LOGICAL      | `.FALSE.`                           | If `.TRUE.`, all S-space and perturbing determinants have orbitals `NDHB1` and `NDHB2` "holed" (for core IP in symmetric systems).                                                                                                                                                                                                                                 |
| `NDHB1`    | INTEGER      | `0`                                 | First spatial core orbital ionized (used with `ZDDH=T`).                                                                                                                                                                                                                                                                                                         |
| `NDHB2`    | INTEGER      | `0`                                 | Second spatial core orbital ionized (used with `ZDDH=T`).                                                                                                                                                                                                                                                                                                        |
| `THRESH`   | REAL\*8      | `0.0`                               | Threshold for writing orbital occupation. Only coefficients > `THRESH` are considered.                                                                                                                                                                                                                                                                           |
| `ZTOUL`    | LOGICAL      | `.FALSE.`                           | If `.TRUE.`, integrals are read from MOTRA+MOLCAS+MOLCSD using Toulouse formalism.                                                                                                                                                                                                                                                                                 |
| `TGEN`     | REAL\*8      | `0.05`                              | Threshold for defining generator determinants when `ZDET=T` and `ZPERTU=F`. A determinant `k` is a generator if any of its coefficients `C(m,k)` in the zero-order states `m` is `> TGEN`.                                                                                                                                                                              |
| `ZDAVID2`  | LOGICAL      | `.FALSE.`                           | If `.TRUE.`, use the `DAVID2` subroutine (alternative Davidson implementation).                                                                                                                                                                                                                                                                                    |
| `HOST`     | CHARACTER    | `'        '`                        | Hostname, possibly for conditional compilation or file paths (not directly used in logic shown).                                                                                                                                                                                                                                                                 |

### `&DIAGO`

| Parameter  | Type          | Default                               | Description                                                                                                                                                                                                                                                                                                                                                       |
| :--------- | :------------ | :------------------------------------ | :---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `SFO(5)`   | REAL\*8 ARRAY | `SFO(1)=1.0D-5`, others `-1.0`        | Convergence threshold for wavefunction in Davidson method. Up to 5 decreasing thresholds `SFO(1)...SFO(5)` can be used in repeated searches.                                                                                                                                                                                                                           |
| `SEN`      | REAL\*8       | `0.0`                                 | Convergence threshold for energy in Davidson method.                                                                                                                                                                                                                                                                                                              |
| `NITER`    | INTEGER       | `50`                                  | Maximum number of iterations for Davidson.                                                                                                                                                                                                                                                                                                                         |
| `MAXM`     | INTEGER       | `12`                                  | Maximum dimension of the subspace diagonalized in Davidson (`MAXM <= ID14`).                                                                                                                                                                                                                                                                                       |
| `MAXGUE`   | INTEGER       | `200`                                 | Number of determinants used to prepare trial vectors (if `ZVEC=F`). Max `600`, must not exceed `(ID7/2)^0.5`.                                                                                                                                                                                                                                                        |
| `IOP`      | INTEGER       | `0`                                   | If `1`, avoid accumulation of errors during subspace dimension reduction (slower). If `0`, normal calculation (faster).                                                                                                                                                                                                                                             |
| `ZVEC`     | LOGICAL       | `.FALSE.`                             | If `.TRUE.`, trial vectors are read from `FILE04` (incompatible with `ZDET=F`) or `FILE14` (see `PROJF`). If `.FALSE.`, prepare trial vectors by diagonalizing a small sub-matrix.                                                                                                                                                                                  |
| `PROJF`    | CHARACTER     | `' '`                                 | File (`FILE14`) from which to read trial vectors for Davidson (must be `FILE04` of a previous run with a subspace of current S). Used only if `ZVEC=T`. If blank, read from current `FILE04`.                                                                                                                                                                        |
| `ZSS`      | LOGICAL       | `.FALSE.`                             | If `.TRUE.`, select trial vectors belonging to a given symmetry.                                                                                                                                                                                                                                                                                                   |
| `CSEL,ISEL`| REAL\*8, INT ARRAY | `0.0`, `0`                          | Representative vector for desired symmetry (coefficients `CSEL`, determinant numbers `ISEL`). Only trial vectors with significant overlap with `CSEL` are retained.                                                                                                                                                                                                 |
| `ZFOL`     | LOGICAL       | `.FALSE.`                             | If `.TRUE.`, the eigenvector for Davidson is determined by its coefficients (preferred if different symmetry vectors are calculated together). If `.FALSE.`, determined by provisional energy order.                                                                                                                                                                |
| `ZWRT`     | LOGICAL       | `.FALSE.`                             | If `.TRUE.`, print intermediate Davidson results.                                                                                                                                                                                                                                                                                                                 |
| `SECR`     | REAL\*8       | `0.05`                                | Threshold for printing eigenvectors (only coefficients > `SECR` are printed).                                                                                                                                                                                                                                                                                      |
| `IETATS(I)`| INTEGER ARRAY | `1, 2, ..., NROT`                     | Indices of states to keep after diagonalization. `IETATS(NROT)` eigenvectors are determined, and `NROT` are chosen by energy order (or modified by L2/S2 calculations if `ZDIALZ=T` or `ZSROT=T`).                                                                                                                                                                   |
| `ZREST`    | LOGICAL       | `.FALSE.`                             | If `.TRUE.`, restart Davidson procedure from a previous calculation (vectors read from `FILE13`).                                                                                                                                                                                                                                                                 |
| `ZSPINSEL` | LOGICAL       | `.FALSE.`                             | If `.TRUE.`, select states based on S^2 eigenvalues to get only desired spin states.                                                                                                                                                                                                                                                                               |
| `SPINMAX,SPINMIN` | REAL\*8 | `20.0`, `-1.0`                        | Upper and lower bounds for S^2 eigenvalue for state selection.                                                                                                                                                                                                                                                                                                     |
| `TOL`      | REAL\*8       | `1.0D-6`                              | (For `DAVID2`) Tolerance for Davidson method.                                                                                                                                                                                                                                                                                                                      |
| `NBX`      | INTEGER       | `50`                                  | (For `DAVID2`) Maximum size of V space.                                                                                                                                                                                                                                                                                                                            |
| `ITMAX`    | INTEGER       | `1000`                                | (For `DAVID2`) Maximum number of iterations.                                                                                                                                                                                                                                                                                                                       |
| `ILEVEL`   | INTEGER       | `2`                                   | (For `DAVID2`) Verbosity level.                                                                                                                                                                                                                                                                                                                                    |
| `NGUE`     | INTEGER       | `400`                                 | (For `DAVID2`) Dimension of guess matrix.                                                                                                                                                                                                                                                                                                                          |
| `NBLOCK`   | INTEGER       | `0`                                   | (For `DAVID2`) Block size.                                                                                                                                                                                                                                                                                                                                             |

### `&DIABA` (Only if `ZDIAB=T`)

Parameters for diabatization by rotation of adiabatic vectors, maximizing overlap with a reference basis.

| Parameter | Type    | Default         | Description                                                                                                                                                                                                                                                                                                                                                                                    |
| :-------- | :------ | :-------------- | :--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `NREF`    | INTEGER | `METAT`         | Number of reference states. Typically `NREF=NROT=METAT`. If `NREF=METAT<NROT`, perturbs quasi-diabatic states that best match references, decoupling them from `NROT-METAT` other states. If `NREF-1=METAT=NROT` (with `ZCOMB=T`), uses a linear combination of the last two references, and `NREF` becomes `NREF-1`.                                                                      |
| `ZCOMB`   | LOGICAL | `.FALSE.`       | If `.TRUE.`, the last two reference states are used in a linear combination, with coefficients determined by maximum overlap.                                                                                                                                                                                                                                                                   |
| `ZLOWD`   | LOGICAL | `.FALSE.`       | If `.TRUE.`, reference states are Löwdin orthogonalized. If `.FALSE.`, only normalization is performed.                                                                                                                                                                                                                                                                                       |
| `ZGRAM`   | LOGICAL | `.FALSE.`       | If `.TRUE.`, reference states are Gram-Schmidt orthogonalized. If `.FALSE.`, only normalization is performed.                                                                                                                                                                                                                                                                                 |
| `ZRD`     | LOGICAL | ` `             | If `.TRUE.`, maximum overlap criterion is modified so that if reference I has no overlap with any adiabatic state, diabatic state I is identified with adiabatic state I (reference order matters).                                                                                                                                                                                              |
| `NREFMO`  | INTEGER | ` `             | Number of orbitals used to construct reference states.                                                                                                                                                                                                                                                                                                                                         |
| `NREFCI`  | INTEGER | ` `             | Number of determinants used to construct reference states.                                                                                                                                                                                                                                                                                                                                       |
| `ZIDEN`   | LOGICAL | `.FALSE.`       | If `.TRUE.`, reference orbitals coincide with MOs used for CI. If `.FALSE.`, read reference orbitals.                                                                                                                                                                                                                                                                                           |
| `IREFMO`  | INTEGER ARRAY | `1..NREFMO` | Numbers of MOs used in reference determinants (only if `ZIDEN=T`).                                                                                                                                                                                                                                                                                                                             |
| `MODISK`  | INTEGER | `5`             | Unit for reading reference MOs (5 or 12).                                                                                                                                                                                                                                                                                                                                                      |
| `MOFORM`  | CHARACTER | `5F14.10`       | Format for reading reference MOs. Reads one vector `(REFMO(I,J),I=1,NAO))` at a time. Special values: `STAR` (free format), `BIN` (binary, only if `MODISK=12`, reads `((REFMO(I,J),I=1,NAO,J=1,NREFMO))`).                                                                                                                                                                                  |
| `IWRT`    | INTEGER | `1`             | Output level (0=minimal, 1=default, 2=extended).                                                                                                                                                                                                                                                                                                                                               |

### `&ATOMO` (Only if `ZATOM=T`)

Parameters for calculating matrix elements of Lx, Ly, Lz in the MO basis.

| Parameter | Type    | Default  | Description                                                                                                                                                               |
| :-------- | :------ | :------- | :------------------------------------------------------------------------------------------------------------------------------------------------------------------------ |
| `IP`      | INTEGER ARRAY | ` `      | Specifies the position of p-functions in the AO basis. For each set of three p-functions, only the number of the first one needs to be specified.                     |
| `ID`      | INTEGER ARRAY | ` `      | Similar to `IP`, but for d-functions.                                                                                                                                 |
| `ATTOL`   | REAL\*8 | `1.0D-6` | Threshold used to define an MO as s, p, or d type based on AO coefficients.                                                                                                 |

## Determinant Input Format (`ZDET=F`)

Determinants for space S are defined by cards, each starting with a character in column 1:

| Code | Excitation Level         |
| :--- | :----------------------- |
| `F`  | No excitation (Reference, doubly occupied) |
| `M`  | Monoexcited determinant  |
| `D`  | Biexcited determinant    |
| `T`  | Triexcited determinant   |
| `Q`  | Quadriexcited determinant|
| `P`  | Pentaexcited determinant |
| `H`  | Hexaexcited determinant  |
| `E`  | Heptaexcited determinant |
| `O`  | Octaexcited determinant  |
| `N`  | Nonaexcited determinant  |
| `X`  | Decaexcited determinant  |
| `U`  | Undecaexcited determinant|
| `Z`  | Dodecaexcited determinant|

Starting from column 3, separated by a single space (or comma), specify the holes, followed by one or more choices of particles.
Holes and particles are generally indicated as spin-orbitals (MO number followed by spin `+` or `-`).
If spin is omitted for mono, bi, and tri-excitations, the program automatically generates all spin parts compatible with `ISZ`.
If spatial symmetry transformations are defined at the integral level, they are applied to all generated determinants, and equivalent determinants need not be entered.
A blank card terminates the sequence.

## Reference Determinant Input (for `ZDIAB=T`)

1.  **Reference MOs:** Cards defining reference MOs (see `MODISK` and `ZIDEN` in `&DIABA`).
2.  **Reference Determinants:**
    *   One card per determinant, indicating occupation of beta spin-orbitals, then alpha spin-orbitals (`T` for occupied, `F` or blank for empty). Sign convention: all beta electrons before all alpha electrons.
3.  **Reference States:**
    *   Card A: Label of the state (columns 1-16).
    *   Cards B: Vector in the basis of reference determinants (format `5F14.10`).

## Key Subroutines

*   **`OFILES`**: Opens and manages files based on `&FILES` namelist.
*   **`DETER`**: Reads determinant definitions, sets up space S, and reads `&ICINP` and `&DIAGO`.
*   **`LECNEWER`**: Reads determinant specifications from input and generates the actual determinants for space S.
*   **`TRANSF`**: Validates excitations and filters determinants.
*   **`SCARA`**: Generates determinants from orbital occupations.
*   **`GENERA`**: Recursively generates spin-adapted configurations.
*   **`STORA`**: Stores generated determinants.
*   **`REIJKL`**: Prepares for storage/retrieval of two-electron integrals.
*   **`ITIJKL`**: Reads and processes two-electron integrals.
*   **`IJKF`**: Computes one-electron (Fock matrix elements) and two-electron (J and K) integrals in MO basis.
*   **`HDIG`**: Calculates diagonal Hamiltonian matrix elements for a determinant.
*   **`HMP`**: Calculates Møller-Plesset energy for a determinant.
*   **`HMAT`**: Constructs the CI Hamiltonian matrix (using `HDIGC` and `HNTDC`).
    *   `HDIGC`: Computes diagonal blocks of H for CSFs.
    *   `HNTDC`: Computes off-diagonal blocks of H between CSFs.
*   **`ESCLASS`**: Classifies determinants into configuration state functions (CSFs) based on open shells.
*   **`MKORB`**: Creates orbital representation for CSF generation.
*   **`DAVID` / `DAVID2`**: Davidson diagonalization algorithm.
*   **`GUESS`**: Generates initial trial vectors for Davidson.
*   **`WRTVAR`**: Writes variational CI results (eigenvalues, eigenvectors).
*   **`ELIM`**: Selects states to keep based on `IETATS`.
*   **`DEGEN`**: Handles symmetrization of degenerate vectors.
*   **`SPIN`**: Calculates S^2 matrix elements and optionally rotates vectors to diagonalize S^2.
*   **`ATOM`**: Calculates L^2 and Lz^2 matrix elements and optionally rotates vectors.
*   **`DIABAT`**: Performs diabatization of adiabatic states.
*   **`PREPER`**: Prepares for the perturbation calculation.
*   **`MKF04`**: Writes variational and preparatory perturbation data to `FILE04`.
*   **`BARI`**: Calculates baricentric energies.
*   **`SINGSUB`**: Handles single substitutions for perturbation theory.
*   **`DOUBSUB`**: Handles double substitutions for perturbation theory.
*   **`PERTU`**: Core perturbation calculation routine.
*   **`WRTDET` / `WRTBIN`**: Writes determinant information (formatted/binary).
*   **`WRTBRD`**: Writes the effective Hamiltonian from QDPT.

## Compilation Notes

*   The code uses preprocessor directives like `#ifdef SINGLET`, `#ifdef DOUBLET`, `#ifdef TRIPLET`, indicating that it can be compiled for different spin multiplicities by defining one of these macros.
*   The program explicitly states it was compiled with the Intel Fortran compiler (`ifc`).

## Maximum Dimensions (Parameters)

These parameters define the fixed-size array limits within the program. They are typically set in an include file like `parameter.cippi`.

| Parameter | Default Value (example) | Description                               |
| :-------- | :---------------------- | :---------------------------------------- |
| `ID1`     | 150                     | Max Molecular Orbitals (MOs)              |
| `ID4`     | 100000                  | Max Determinants in zero-order space S    |
| `ID5`     | 100000                  | Max Determinants read from `FILE05` / Max Generator Determinants |
| `ID6`     | 18                      | Max States Treated                        |
| `ID7`     | 110001                  | Max (States * Determinants)               |
| `ID8`     | 650000                  | Max Holes and Particles                   |
| `ID13`    | 150                     | Max Atomic Orbital Basis (for ATOM/DIAOVL)|
| `ID14`    | 30                      | Max Trial Vectors (Davidson)              |
| ...       | ...                     | Other excitation level limits (HEXA, EPTA, etc.) are also defined based on `ID4` and `ID8`. |

The actual values for these `IDx` parameters are taken from `parameter.cippi` at compile time.
```
