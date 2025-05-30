```markdown
# `dypc2.F` Documentation

## Program Name

`dypc2.F` (The program itself seems to be the main driver for NEVPT2 calculations, not a library of subroutines named DYPC2 or DYPAUX). The name likely stands for Dyall's Hamiltonian Perturbation Correction to 2nd order.

## Description

`dypc2.F` is a FORTRAN program designed to calculate the second-order perturbation energy correction based on the "n-electron valence state perturbation theory" (NEVPT2), specifically using the partially contracted formulation. It is intended to work with reference wavefunctions that are S^2 eigenstates and utilizes Dyall's Hamiltonian.

The program computes various components of the NEVPT2 energy arising from different classes of excitations involving core, active, and virtual orbitals.

## Core Functionality

*   Calculates second-order NEVPT2 energy corrections.
*   Employs a partially contracted scheme.
*   Assumes the reference wavefunction is an S^2 eigenstate.
*   Uses Dyall's Hamiltonian formulation.
*   Computes distinct energy contributions from different excitation classes:
    *   `V(0)`: Perturbations involving only inactive (core and virtual) orbitals (double excitations from core to virtual).
    *   `V(+1)`: Perturbations involving one active electron promoting to a virtual orbital.
    *   `V(-1)`: Perturbations involving one core electron promoting to an active orbital.
    *   `V(+2)`: Perturbations involving two active electrons promoting to virtual orbitals.
    *   `V(-2)`: Perturbations involving two core electrons promoting to active orbitals.
    *   `V(0')`: Semi-internal perturbations involving one active electron excited and another active electron changing its state within the active space, coupled with core/virtual excitations.
    *   `V(+1')` and `V(-1')`: Perturbations involving excitations between core/virtual spaces mediated by the active space.
*   Optionally calculates the perturbed density matrix.
*   Optionally stores data for third-order NEVPT calculations.

## Input Files

*   **Unit 3 (`FILE03` or user-defined in `&FILES`):** A copy of the NAMELIST input read from Unit 5.
*   **Unit 4 (`FILE04` or user-defined in `&FILES`):**
    *   Contains data from a preceding CI (or CASSCF) calculation, such as CI coefficients, energies, determinant definitions, orbital specifications (`NORB`, `NOCB`, `NOCA`, `METAT`, `NCF`, etc.), and integral handling information (`FACTOR`, `ESCF`). This file is crucial for defining the reference zeroth-order wavefunction and its energy.
*   **Unit 5 (Standard Input):**
    *   Title card.
    *   Namelist `&FILES`.
    *   Namelist `&ICINP`.
*   **Unit 25 (`IJKL.FILE25` or user-defined in `&FILES`):**
    *   Orbital information: `NSYM` (number of symmetries), `NORB` (number of orbitals), `NOC` (number of occupied core/active orbitals in reference), `NDEGEN` (degeneracy info), `ITSYM` (symmetry of each orbital), `NAO` (number of atomic orbitals), `NGELO` (number of frozen core orbitals), `NGELV` (number of frozen virtual orbitals).
    *   Symmetry multiplication table `ITS`.
*   **Unit 26 (Optional, `FILE26` in `&FILES`):**
    *   Used if `ZTOUL=T` (Toulouse integral formalism). Contains one-electron integrals from a MOLCSD calculation.
*   **Unit 31 (`KOOP.FILE31` or user-defined in `&FILES`):**
    *   Contains spin-less one- and two-particle density matrices (DA, DAA, DAAT) for the active space.
    *   Contains generalized Koopmans matrices (GKP1A, GKM1A, GKP2AA, GKM2AA, GK0PA, GK0PD, GK0PF).
    *   Contains the three-particle density matrix (RO3) and related matrices (AMAT, BMAT, CMAT, DMAT) for `V(+/-1')` terms.
*   **Unit 50 (`IJKL.FILE50` or user-defined in `&FILES`):**
    *   Two-electron integrals in the MO basis.

## Output Files

*   **Unit 6 (Standard Output):** Program log, input parameters, summary of NEVPT2 energy components (SC-NEVPT and PC-NEVPT versions for each class), total second-order correction, and final energies.
*   **Unit 27 (`RHOFILE`, if `ZRO=T` in `&FILES`):**
    *   Stores perturbed density matrix components: `RHO(i,r)` (core-virtual), `RHO(d,r)` (active-virtual), `RHO(i,d)` (core-active).
*   **Unit 32 (`FILE32`, if `ZTHIRD=T` in `&ICINP`):**
    *   Stores intermediate data required for a subsequent third-order NEVPT calculation.

## NAMELIST Parameters

### `&FILES` (Read from Unit 5)

| Parameter  | Type    | Default         | Description                                                                                                                                                                                                                            |
| :--------- | :------ | :-------------- | :------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `FILE03`   | CHARACTER | Derived from `FILE04` or `CIPPI.FILE03` | Output file for determinant definitions (standard CIPPI behavior, less relevant for dypc2 directly as it reads processed CI info).                                                                             |
| `FILE04`   | CHARACTER | `CIPPI.FILE04`  | Main input file containing reference wavefunction data from a previous CI run.                                                                                                                                                         |
| `FILE07`   | CHARACTER | `' '`           | Not directly used by dypc2 for its primary calculation but listed for consistency with CIPPI modules.                                                                                                                                |
| `FILE09`   | CHARACTER | `CIPPI.FILE09`  | Unformatted scratch file.                                                                                                                                                                                                              |
| `FILE11`   | CHARACTER | `' '`           | Not directly used by dypc2 but listed.                                                                                                                                                                                                 |
| `FILE12`   | CHARACTER | `' '`           | Not directly used by dypc2 but listed.                                                                                                                                                                                                 |
| `FILE13`   | CHARACTER | `DIAGO.RESTART` | Not directly used by dypc2 but listed (Davidson restart file).                                                                                                                                                                         |
| `FILE20`   | CHARACTER | `CIPPI.FILE20`  | Not directly used by dypc2 but listed (CI matrix input file).                                                                                                                                                                          |
| `FILE25`   | CHARACTER | `IJKL.FILE25`   | Input for orbital symmetry and count information.                                                                                                                                                                                      |
| `FILE26`   | CHARACTER | `' '`           | Input for one-electron integrals if `ZTOUL=T`.                                                                                                                                                                                         |
| `FILE31`   | CHARACTER | `KOOP.FILE31`   | Input file for density matrices and Koopmans matrices.                                                                                                                                                                                 |
| `FILE32`   | CHARACTER | Default name    | Output file for third-order NEVPT data if `ZTHIRD=T`.                                                                                                                                                                                  |
| `FILE50`   | CHARACTER | `IJKL.FILE50`   | Input for two-electron integrals.                                                                                                                                                                                                      |
| `ZVERBOSE` | LOGICAL   | `.FALSE.`       | If `.TRUE.`, intermediate results are printed.                                                                                                                                                                                         |
| `ZRO`      | LOGICAL   | `.FALSE.`       | If `.TRUE.`, the perturbed density matrix (Partially Contracted version) is calculated and its components are printed to `RHOFILE` (Unit 27).                                                                                          |

### `&ICINP` (Read from Unit 5)

| Parameter | Type    | Default   | Description                                                                                                                                                           |
| :-------- | :------ | :-------- | :-------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `ZTOUL`   | LOGICAL | `.FALSE.` | If `.TRUE.`, integrals are read from MOTRA+MOLCAS+MOLCOST using Toulouse formalism.                                                                                   |
| `FROM`    | CHARACTER*6 | `'      '` | If `'DALTON'`, Koopmans matrices read from `FILE31` are scaled by `FACTOR`.                                                                                             |
| `ZTHIRD`  | LOGICAL | `.FALSE.` | If `.TRUE.`, data needed for the third-order NEVPT calculation are stored in `FILE32`. If `.FALSE.`, `FILE32` is not generated.                                          |
| `ZPRT`    | LOGICAL | `.FALSE.` | If `.TRUE.`, verbose printing to standard output. If `.FALSE.`, standard (concise) output. (This is distinct from `ZVERBOSE` in `&FILES` which controls other prints). |
| `NCORE`   | INTEGER | `-1`      | Number of doubly occupied inactive (core) orbitals. If `-1` and `NACT=0`, active space is inferred from `FILE04`. If `NACT > 0`, `NCORE` must be set.                  |
| `NACT`    | INTEGER | `0`       | Number of active orbitals. If `0` and `NCORE=-1`, active space is inferred from `FILE04`.                                                                                |
| `IGELO`   | INTEGER ARRAY | None    | Indices of doubly occupied orbitals to be kept inactive (frozen) in the perturbative part. These orbitals are excluded from excitations.                           |

## Key Computational Steps

1.  **Initialization:**
    *   Print program banner.
    *   Call `OFILES` to handle file assignments based on `&FILES` namelist.
    *   Read global parameters (`NSYM`, `NORB`, `NOC`, etc.) from Unit 25.
    *   Read reference wavefunction information (CI energies, coefficients, determinant structure, `FACTOR`, `ESCF`) from Unit 4.
    *   Read `&ICINP` namelist (`ZTOUL`, `NCORE`, `NACT`, `IGELO`, etc.).
2.  **Integral Processing:**
    *   Call `REIJKL` to set up integral processing metadata.
    *   Call `ITIJKL` (or `ITIJKLt` if `ZTOUL=T`) to read and process two-electron integrals from Unit 50, storing them internally (e.g., in `IJKL4` common block).
    *   Read one-electron integrals (`ONEL` common block) either from Unit 26 (if `ZTOUL=T`) or from Unit 50 (standard HONDO format).
3.  **Orbital Space Definition:**
    *   Call `DEFORB` to define core, active, and virtual orbital spaces based on `NCORE`, `NACT`, and `ZACT` (activity flags from Unit 4 or derived from `NCORE`/`NACT`).
    *   Process `IGELO` to mark frozen core orbitals.
4.  **Density and Koopmans Matrix Input:**
    *   If `NACT > 0`, read active-space density matrices (`DA`, `DAA`, `DAAT`) and various Koopmans matrices (`GKP1A`, `GKM1A`, `GKP2AA`, `GKM2AA`, `GK0PA`, `GK0PD`, `GK0PF`, `RO3`, `AMAT`, `BMAT`, `CMAT`, `DMAT`) from Unit 31. These are essential for the NEVPT2 terms involving active orbitals.
5.  **Effective Fock Matrix Elements (`EPS`):**
    *   Calculate effective one-electron energies (`EPS(i)`) for core and virtual orbitals, dressed with interactions with core and active electrons (using `DA` if `NACT > 0`). These are stored in `FMPB`.
6.  **NEVPT2 Energy Calculation:**
    *   **`V(0)` term:** Calculated by subroutine `V0`. This term involves excitations from core to virtual orbitals.
    *   **Terms involving active orbitals (if `NACT > 0`):**
        *   `V(+1)` term: Calculated by `V1K`.
        *   `V(-1)` term: Calculated by `VM1K`.
        *   `V(+2)` term: Calculated by `V2MOD`.
        *   `V(-2)` term: Calculated by `VM2MOD`.
        *   `V(0')` term: Calculated by `V0PMOD`.
        *   `V(-1')` term: Calculated by `VMUPE`.
        *   `V(+1')` term: Calculated by `VPUPE`.
    *   Each of these routines computes both the "Strongly Contracted" (SC) contribution (summed into `E2MPP`, `PSIMPP`) and the "Partially Contracted" (PC) contribution (summed into `E2ENP`, `PSIENEP`). The total SC and PC energies are accumulated in `E2MP(1)`/`PSIMP(1)` and `E2EN(1)`/`PSIEN(1)` respectively.
7.  **Results Summary:**
    *   Print a detailed summary of SC and PC energy contributions from each class of perturbers (`(0)`, `(+1)`, `(-1)`, etc.).
    *   Print total SC-NEVPT2 and PC-NEVPT2 corrections and the final energies (Reference Energy + Correction).
8.  **Optional Density Matrix Calculation:**
    *   If `ZRO=T`, calculate and write perturbed density matrix components to Unit 27. This involves reading back intermediate contributions and using coefficients from the Koopmans matrix diagonalizations.

## Common Blocks

*   **/CIP/**: General CI program parameters (SCF energy, thresholds, orbital counts, logical flags for program control).
*   **/VEC/**: Stores energies from the reference calculation (`E`), Epstein-Nesbet and Møller-Plesset reference energies (`EEN`, `EMP`), and their second-order corrections (`E2EN`, `E2MP`). Also stores norms of first-order wavefunction corrections (`PSIMP`, `PSIEN`). `FMPB` stores effective Fock energies.
*   **/DET/**: Information about determinants in the reference space (number of excitations `NE`, starting indices `ND`, hole `TROU` and particle `PART` orbital indices, orbital symmetries `ITSYM`, symmetry multiplication table `ITS`).
*   **/HND/**: Flags for integral handling (`ZHANDY`, `ZTHREE`, `ZMOTRA`, `ZFAC`).
*   **/INT/**: Metadata for integral storage and retrieval (`NUM`, `NDEB`, `NAD`, `KT`, `INDIC`, `JNDIC`, `LNDIC`). `NI4`, `NI3`, `NI2`, `NR4` define sizes of integral arrays.
*   **/PERTURBER/**: Stores one-electron integrals (`ONEL`), components of PC-NEVPT2 energy (`E2ENP`, `PSIENEP`) and SC-NEVPT2 energy (`E2MPP`, `PSIMPP`) for each class of perturber, frozen orbital indices (`IGELO`), and flags `ZVERBOSE`, `ZRO`.
*   **/MEM/**: Dynamic memory allocation tracking.
*   **/CPU/**: CPU timing.
*   **/DEBUG/**: Debugging flags.
*   **/FIL/**: File unit associations.
*   **/DEEPHOLE/**: Parameters for deep hole calculations (not actively used in `dypc2.F` logic shown).
*   **/TOUL/**: Flag `ZTOUL` for Toulouse integral format.
*   **/DETINF/**: Stores total counts of determinants, configurations, and capostipiti; `ZACT` stores activity flags for orbitals.
*   **/THRESHO/**: Threshold for printing orbital occupations.
*   **/BIG/**: Large arrays for storing integrals (`IJKL2`, `IJKL4`, `IJKL3`, `RIJKL`) managed via pointers.

## Called Subroutines (External/Library)

The program `dypc2.F` itself is a main program. It calls numerous subroutines which are assumed to be linked from other files or libraries. Key computational routines include:

*   **`V0`**: Calculates `V(0)` NEVPT2 energy component.
*   **`V1K`**: Calculates `V(+1)` NEVPT2 energy component.
*   **`VM1K`**: Calculates `V(-1)` NEVPT2 energy component.
*   **`V2MOD`**: Calculates `V(+2)` NEVPT2 energy component.
*   **`VM2MOD`**: Calculates `V(-2)` NEVPT2 energy component.
*   **`V0PMOD`**: Calculates `V(0')` NEVPT2 energy component.
*   **`VMUPE`**: Calculates `V(-1')` NEVPT2 energy component.
*   **`VPUPE`**: Calculates `V(+1')` NEVPT2 energy component.
*   **Utility routines (likely from CIPPI package):** `OFILES`, `REIJKL`, `ITIJKL`, `ITIJKLt`, `AI` (accesses two-electron integrals), `HANDY`, `PICK4`, `DEFORB`, `FILLCM2`, `FILLCM2P`, `PROVASYMM`, `DSYGV`, `DSYEV`, `DGEMM`, `DAXPY`, `DCOPY`, `DDOT`, `ZEROE`, `MATOUT`, `CPUT`, `LECNAM`, `NAMELx`, `CERCA`, `FILESPLIT`.

This documentation should provide a good overview of the `dypc2.F` program's functionality and usage.
```
