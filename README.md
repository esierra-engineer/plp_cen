# PLP (Stochastic Hydrothermal Dispatch with SDDP)

PLP is a mixed **Fortran + C++** power-system optimization program used for hydrothermal coordination, economic dispatch, and related operational variables using **Stochastic Dual Dynamic Programming (SDDP)**.

The repository includes:
- `PLPv6.3/`: PLP source code and CMake build
- `CLPv1.16-CDEC/`: bundled COIN-OR CLP distribution
- `OSI-CDECv1.2/`: bundled COIN-OR OSI distribution
- `run_plp.sh`: typical runtime launcher used in a scenario folder

---

## 1) Requirements

Minimum Linux toolchain:
- `gcc`, `g++`, `gfortran`
- `make`, `cmake`
- `zlib` development files
- BLAS/LAPACK (or equivalent numeric libraries)
- `pthread` and `libm` (normally system-provided)

Typical package set (Debian/Ubuntu):
```bash
sudo apt update
sudo apt install -y build-essential gfortran cmake zlib1g-dev liblapack-dev libblas-dev dos2unix
```

Typical package set (RHEL/CentOS-like):
```bash
sudo yum groupinstall -y "Development Tools"
sudo yum install -y gcc-gfortran cmake zlib-devel lapack-devel blas-devel dos2unix
```

---

## 2) Install and compile dependencies

PLP links through COIN-OR OSI. You can run with:
- **CLP** (`OsiClp`)
- **CPLEX** (`OsiCpx`, CPLEX already installed)
- **GUROBI** (`OsiGrb`, Gurobi already installed)

> Recommended install prefix for COIN libs in this repo: `/opt/coinor` (matches default `COIN_ROOT_DIR` in `PLPv6.3/CMakeLists.txt`).

### 2.1 Build CLP (bundled)

```bash
cd <repo_root>/CLPv1.16-CDEC
./configure --prefix=/opt/coinor
make -j"$(nproc)"
make install
```

### 2.2 Build OSI for CPLEX or GUROBI (bundled)

If you will use CPLEX/GUROBI through OSI, build OSI with the corresponding solver installed first.

```bash
cd <repo_root>/OSI-CDECv1.2/rev_0_106_20140630
./configure --prefix=/opt/coinor
make -j"$(nproc)"
make install
```

If autodetection fails, set solver include/library paths in your environment and rerun configure.

### 2.3 OSI + modern CPLEX compatibility notes

This repository ships an **old OSI fork** (`OSI-CDECv1.2`, 2014-era code). For modern CPLEX versions, watch for:
- legacy parameter/API assumptions in `OsiCpxSolverInterface` (`CPX_PARAM_*`, `CPX_VERSION` guards)
- hardcoded search paths in `PLPv6.3/cmake/FindCPLEX.cmake` (`lib/x86-64_linux/static_pic`)
- static-library naming/layout differences in recent IBM ILOG CPLEX packages

Practical recommendation:
- keep a validated CPLEX + OSI toolchain snapshot for production
- if using a newer CPLEX release, test a full PLP build/run early and patch finder/interface files if needed

---

## 3) Compile PLP

### CLP backend
```bash
cd <repo_root>/PLPv6.3
cmake -S . -B build-clp \
  -DCOIN_ROOT_DIR=/opt/coinor \
  -DCOIN_USE_CLP=ON -DCOIN_USE_CPX=OFF -DCOIN_USE_GRB=OFF \
  -DCMAKE_BUILD_TYPE=Release
cmake --build build-clp -j"$(nproc)"
```

### CPLEX backend
```bash
cd <repo_root>/PLPv6.3
cmake -S . -B build-cpx \
  -DCOIN_ROOT_DIR=/opt/coinor \
  -DCPLEX_ROOT_DIR=/opt/cplex/cplex \
  -DCOIN_USE_CPX=ON -DCOIN_USE_CLP=OFF -DCOIN_USE_GRB=OFF \
  -DCMAKE_BUILD_TYPE=Release
cmake --build build-cpx -j"$(nproc)"
```

### GUROBI backend
```bash
cd <repo_root>/PLPv6.3
cmake -S . -B build-grb \
  -DCOIN_ROOT_DIR=/opt/coinor \
  -DGUROBI_ROOT_DIR=/opt/gurobi \
  -DCOIN_USE_GRB=ON -DCOIN_USE_CLP=OFF -DCOIN_USE_CPX=OFF \
  -DCMAKE_BUILD_TYPE=Release
cmake --build build-grb -j"$(nproc)"
```

---

## 4) Run a scenario

Typical production pattern:
1. Copy the selected PLP binary to your runtime location (commonly `/opt/plp/...`).
2. Enter a scenario directory containing `.dat` input files.
3. Execute `run_plp.sh` from that scenario directory.

Example:
```bash
cd /path/to/scenario_with_dat_files
<repo_root>/run_plp.sh
```

`run_plp.sh` exports runtime environment flags (cuts, scaling, irrigation conventions, tolerances, parallel mode) and then executes the configured PLP binary.

---

## 5) Input file reference (all known names in source)

### 5.1 Input format conventions

- Primary model inputs are text files, mostly `*.dat`.
- Lines beginning with `#` are comments.
- Values are read as fixed/list-directed numeric/text fields depending on each reader routine.
- Some optional integrations use `*.csv`.

### 5.2 Input files

| File | Type | Purpose / subsystem |
|---|---|---|
| `plppar.dat` | DAT | Global model parameters |
| `plppath.dat` | DAT | Path/runtime control file |
| `plprun.dat` | DAT | Run configuration / run identifiers |
| `plpeta.dat` | DAT | Stage/time-step definition |
| `plpblo.dat` | DAT | Intra-stage block definition |
| `plpidsim.dat` | DAT | Simulation/scenario index mapping |
| `plpidape.dat`, `plpidap2.dat` | DAT | SDDP sampling/stage index data |
| `plpbar.dat` | DAT | Bus/bar data |
| `plpcnfli.dat` | DAT | Network line configuration |
| `linconf.csv` | CSV | Alternate/aux line configuration |
| `plpcnfce.dat` | DAT | Plant/central configuration (core generator catalog) |
| `centipo.csv` | CSV | Optional central type override for outputs |
| `plpcosce.dat` | DAT | Plant variable costs and related economics |
| `plpdem.dat` | DAT | Demand by stage/block |
| `plpmat.dat` | DAT | Matrix-level model options/penalties (includes spill costs) |
| `plpafpar.dat`, `plpaflce.dat` | DAT | Hydrology/inflow parameters and inflow by plant |
| `plpcenfi.dat`, `plpfilemb.dat` | DAT | Embalse filters and filemb linkage |
| `plpvrebemb.dat` | DAT | Spill/rebound conditions for reservoirs |
| `plpqebnd.dat` | DAT | Reservoir outflow bounds |
| `plpcenre.dat`, `plpcenpmax.dat` | DAT | Generation efficiency and max-power overrides |
| `plpextrac.dat` | DAT | External transfer/extraction settings |
| `plpmance.dat`, `plpmances.dat` | DAT | Plant maintenance (standard/series) |
| `plpmanem.dat`, `plpmanems.dat`, `plpminembh.dat` | DAT | Reservoir maintenance and minimum storage controls |
| `plpmanli.dat`, `mantlin.csv`, `mantlins.csv` | DAT/CSV | Line maintenance schedules |
| `plplaja.dat`, `plplajan.dat`, `plplajam.dat` | DAT | Laja irrigation convention variants |
| `plpmaule.dat`, `plpmaulen.dat` | DAT | Maule irrigation convention variants |
| `plpriego.dat` | DAT | Irrigation-related operational data |
| `plpralco.dat` | DAT | Ralco-specific restriction inputs |
| `plpplem1.dat`, `plpplem2.dat` | DAT | Initial embalse plan inputs |
| `plpcnfgnl.dat`, `plpcnfgn.dat` | DAT | GNL / GN restrictions |
| `plpcnfres.dat` | DAT | Reserve services configuration |
| `plpmanresc.dat`, `plpmanresg.dat`, `plpmanresz.dat` | DAT | Reserve maintenance/config controls |
| `plpcosresc.dat` | DAT | Reserve scarcity / reserve cost inputs |
| `plpcenbat.dat`, `plpmanbat.dat` | DAT | Battery modeling and battery maintenance |
| `plpendes.dat`, `plpendci.dat` | DAT | Legacy/compatibility inputs used by specific workflows |

> Additional legacy names (`pcp*.dat`, `plphid.dat`) appear in comments/compatibility paths and can surface in historical datasets.

---

## 6) Output file reference

### 6.1 Output format conventions

- Main outputs are `CSV` files (one per subsystem/component group).
- Logs and debug traces are plain text (`.log` and console/stdout).
- Some outputs are conditional on enabled features (reserves, batteries, Laja/Maule conventions, failure generation).

### 6.2 Output files

| File | Type | Description |
|---|---|---|
| `plpcen.csv` | CSV | Generation by plant/central |
| `plpcostop.csv` | CSV | Operating cost breakdown |
| `plpser.csv` | CSV | Series/hydraulic chain generation results |
| `plpextrac.csv` | CSV | External transfers/extractions |
| `plpbar.csv` | CSV | Bus-level results (e.g., marginal costs) |
| `plplin.csv` | CSV | Transmission line flow-related outputs |
| `plpemb.csv`, `plpembe.csv` | CSV | Reservoir state/energy results |
| `plpplaem.csv` | CSV | Embalse planning outputs |
| `plpplanos.csv`, `planos.csv` | CSV | Cut/plane related exports |
| `plplaja.csv`, `plplaja1.csv`, `plplaja2.csv` | CSV | Laja convention outputs |
| `plpmaule.csv`, `plpmaul1.csv`, `plpmaul2.csv`, `plpmaul3.csv`, `plpmaul4.csv` | CSV | Maule convention outputs |
| `plpgnl.csv`, `plpgn.csv` | CSV | GNL/GN restriction outputs |
| `plpcostpres.csv` | CSV | Reserve scarcity/penalty outputs |
| `plpres.csv`, `plpresz.csv` | CSV | Reserve outputs by plant and zone |
| `plpbat.csv` | CSV | Battery operation and state-of-charge outputs |
| `plpfal.csv` | CSV | Failure-generation tracking |
| `plpwarn.log`, `plpfact.log`, `plpdeb.log` | LOG | Warnings, feasibility-cut/debug traces |
| `fecha-corrida` | TXT | Run timestamp/mark file |

Auxiliary CSV artifacts used in some workflows: `barras.csv`, `centrales.csv`, `embalses.csv`, `etapas.csv`, `lineas.csv`, `presuf.csv`, `series.csv`, `simuls.csv`, `costosop.csv`.

---

## 7) Technical structure

PLP architecture in this repository:

- **Core engine (Fortran)**: `PLPv6.3/src/*.f`, `*.F`, `*.f90`
  - model data ingestion (`lee*.f`)
  - matrix generation (`genmat*`, `pdmat*`, `defprbpd.f`)
  - SDDP phases (`plp-faseprim.f`, `plp-fasedual.f`, transfers/convergence)
  - output extraction/writing (`plp-gdbd*.f`, `plp-gradat.f`)
- **Solver bridge (C++ + Fortran interface)**:
  - `osicallsc.cpp/.hpp/.f90` provide LP operations through OSI
  - supports `OsiClp`, `OsiCpx`, `OsiGrb`
- **Build system**:
  - CMake in `PLPv6.3/CMakeLists.txt`
  - custom find modules in `PLPv6.3/cmake/`
- **Runtime wrappers**:
  - root `run_plp.sh`
  - solver-specific runner variants in `PLPv6.3/runners/`

---

## 8) Roadmap context for future improvements

This documentation is intended to support upcoming engineering sessions focused on:
- adding new operational and market functionalities
- improving I/O throughput and data handling efficiency
- accelerating heavy kernels (including CUDA-capable paths where applicable)
- reducing technical debt in old solver interfaces
- improving portability (including eventual Windows support)
