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

## 5) Input file reference

### 5.1 General format conventions

- All primary inputs are plain-text files with the `.dat` extension.
- Lines read with Fortran list-directed I/O (`READ(unit, *)`) ignore leading whitespace.
- Comment/label lines are single characters read as `CHARACTER*12` with format `'(A1)'` or `'(A12)'` and discarded — they are not `#`-prefixed in source but rather blank separator lines or short labels.
- Count lines (number of elements) always appear right after a separator, before the data rows.
- Some optional features accept `.csv` files (identified explicitly in the table below).

> **Rule of thumb:** every section in a `.dat` file is preceded by at least one "header/comment" line that is thrown away. The actual data follows on free-format lines (`READ(unit, *)`).

---

### 5.2 Input file details and format examples

#### `plpeta.dat` — Stages / time-steps

```
# (separator line, consumed and discarded)
# (second separator)
48 M                                  ! NEtapa   UnidTiempo (H/D/S/M)
# (separator)
  1 2024  1  1 F  730  0.04  HIDRO    ! Year Mes NumEta FDepHid DurEta Tasa TipoEtapa
  2 2024  2  2 F  672  0.04  HIDRO
  ...
```

Columns in the data rows:

| Column | Type | Description |
|---|---|---|
| Year | INT | Calendar year |
| Mes | INT | Month (1–12) |
| NumEta | INT | Sequential stage index (1-based, must match row order) |
| FDepHidEta | LOGICAL | Flag: hydraulic stage dependency |
| DurEta | INT | Duration in time units (hours/days/weeks/months) |
| Tasa | REAL | Discount rate factor for this stage |
| TipoEtapa | CHAR | Stage-type label (e.g. `HIDRO`, `SECO`) used in output CSV columns |

`UnidTiempo` controls the time unit: `H`=hours, `D`=days, `S`=weeks, `M`=months.

---

#### `plpblo.dat` — Intra-stage blocks

```
# (separator)
# (separator)
96                                     ! NBloque
# (separator)
  1  1  365.0                          ! NumBlo  NumEta  DurBlo
  2  1  365.0
  ...
```

The total duration across all blocks must equal the sum of stage durations. If this file is absent, PLP defaults to one block per stage.

---

#### `plpbar.dat` — Buses/nodes

```
# (separator)
# (separator)
4                                      ! NBarra
# (separator)
  1  ANGOSTURA                         ! NBar  BarNom (up to 48 chars)
  2  ITAHUE
  3  ANCOA
  4  CIPRESES
```

---

#### `plpcnfce.dat` — Generator (plant) configuration

This is the most complex input file. Its structure is:

```
# (separator)
# (separator)
NCentral NCenEmb NCenSer NCenFalla NCenPas NCenBat   ! totals by type
# (separator)
FCenInter FMinTec FCenCAD FFaseSinMT EtaIniSinMT     ! global flags

# (separator – reservoir plants start)
# (separator)
  # (separator – one per plant)
  CenInd CenNom CenIPot CenMinTec CenInter CenFCAD CenMTTdHrz EstocFIndep
  # (generation interval)
  PotMin PotMax VertMin VertMax
  # (startup/shutdown costs)
  CArr  CDet  COn
  # (other data)
  CosVar Ren GBar GHid1 VHid1 PIni CauAfl VolIni VolFin VolMin VolMax FEsc CFUE

# (separator – run-of-river series plants start)
  ... (similar block, without vol fields)

# (separator – run-of-river pasada plants start)
  ... (PotMin PotMax only, + CosVar Ren GBar ...)

# (separator – thermal/wind/solar plants start)
  ... (PotMin PotMax, CosVar Ren GBar)

# (separator – failure plants start)
  ...

# (separator – batteries start)
  ...
```

Key fields for reservoir (Embalse) plants:

| Field | Description |
|---|---|
| `CenInd` | External integer identifier used in extraction/cross-reference files |
| `CenNom` | Plant name (up to 48 chars) |
| `CenIPot` | Installed-power index (number of generation intervals) |
| `PotMin/PotMax` | Active-power generation bounds [MW] |
| `VertMin/VertMax` | Spillage/discharge bounds [m³/s] |
| `CArr/CDet/COn` | Startup cost [k$/start], shutdown cost, initial state flag |
| `CosVar` | Variable O&M cost [$/MWh] |
| `Ren` | Hydraulic efficiency factor |
| `GBar` | Index of the bus where this plant is connected (0 = disconnected from network) |
| `GHid1/VHid1` | Hydraulic topology: downstream plant index, volume connection index |
| `CauAfl` | Base inflow [m³/s] |
| `VolIni/VolFin/VolMin/VolMax` | Initial, final, min, max reservoir volumes [Hm³] |
| `FEsc` | Volume scaling factor (Hm³ → internal units) |
| `CFUE` | Logical flag: forces end-of-horizon volume |

---

#### `plpcnfli.dat` — Transmission lines

```
# (separator)
# (separator)
3 T M 0.0                              ! NLinea  FPerdTram  FPerdLin  ThetaRef
# (separator)
# (separator)
ANCOA-CIPRESES   1.0  1.0  3  4  220.0  0.0  0.03  T  1  T  F
CIPRESES-ITAHUE  1.0  1.0  4  2  220.0  0.0  0.02  T  1  T  F
ITAHUE-ANGOST    1.0  1.0  2  1  220.0  0.0  0.04  T  1  T  F
```

Data columns per line:

| Field | Description |
|---|---|
| `LinNom` | Line name (up to 48 chars) |
| `LinAB`, `LinBA` | Loss factors from A→B and B→A |
| `LinNBar(1)`, `LinNBar(2)` | From-bus and to-bus indices |
| `LinVNom` | Nominal voltage [kV] |
| `LinRes` | Resistance [p.u.] |
| `LinXImp` | Reactance [p.u.] |
| `LinFPer` | Flag: losses modeled for this line |
| `LinNFlu` | Number of sub-flows (parallel circuits counted as one line with multiple flows) |
| `FOpe` | Logical: line in service |
| `LinHVDC` | Logical: HVDC line (DC model) |

`FPerdTram` (global): enable loss model. `FPerdLin` loss allocation: `E`=to emitter, `R`=to receiver, `M`=split 50/50. `ThetaRef` is the reference angle [rad].

---

#### `plpdem.dat` — Demand by bus

```
# (separator)
# (separator)
2                                      ! NBarDem (number of buses with demand)
# (separator)
ANGOSTURA                              ! bar name
# (separator)
4                                      ! NBloDem (number of block entries)
# (separator)
  1  1  800.0                          ! NDia  NumBlo  PotDem [MW]
  1  2  700.0
  ...
# (separator)
ITAHUE
  ...
```

The `NDia` column is read but currently unused; `NumBlo` identifies the block index (must be 1..NBloque); `PotDem` is added to the bus load for that block [MW].

---

#### `plpcosce.dat` — Variable costs by plant/stage

```
# (separator)
# (separator)
3                                      ! NCenCos (plants with non-default costs)
# (separator)
NEHUENCO_U1                            ! plant name
# (separator)
12                                     ! NEtaCos
# (separator)
  1  1   35.20                         ! NDia  NumEta  CosVar [$/MWh]
  1  2   35.20
  ...
```

Costs are stored per stage and converted to internal units via `FactTiempo`. `NDia` is read but unused for compatibility; `NumEta` addresses the stage index; `CosVar` is in $/MWh.

---

#### `plpmat.dat` — Solver control and penalty costs

```
# (separator)
# (separator)
200  1e-6  0.05                        ! PDMaxIte  PDError  UmbIntConf
# (separator)
100  1e-5                              ! PMMaxIte  PMError
# (separator)
0.0  0.05  2000.0  200.0  0.0  0.01  F  F  F   ! Lambda CTasa CCaudFalla CVertimiento CInter CTrasmision FVolFinEmb FPreProc FPrevia
# (separator)
F  F  T  F                             ! FFixTrasm FSeparaFCF FGrabaCSV FGrabaRES
# (separator)
50  1e-9  0                            ! ABLMax  ABEpsilon  NumEtaCF
# (separator)
F  F  1e-6  1e-6                       ! FConvPGradx FConvPVar UmbGradX UmbZSPF
```

Key parameters:

| Parameter | Description |
|---|---|
| `PDMaxIte` | Maximum SDDP dual iterations |
| `PDError` | Convergence tolerance for SDDP optimality gap |
| `UmbIntConf` | Confidence interval threshold |
| `PMMaxIte` | Maximum simulation passes |
| `Lambda` | Global discount rate (0 = flat horizon) |
| `CCaudFalla` | Failure-generation cost [$/m³/s] (converted to internal units) |
| `CVertimiento` | Spill penalty cost [$/m³/s] |
| `CTrasmision` | Network congestion cost |
| `FVolFinEmb` | Force final reservoir volume to target |
| `FGrabaCSV` | Write detailed per-simulation CSV output files |
| `FGrabaRES` | Write reserve-related output |
| `ABLMax` | Max Benders-cut accumulation depth |
| `ABEpsilon` | Cut acceptance tolerance |
| `NumEtaCF` | Number of stages for feasibility-cut region |
| `FConvPGradx` | Use gradient-based convergence criterion |
| `UmbGradX` | Gradient convergence threshold |

---

#### `plpidsim.dat` — Simulation index

```
# (separator)
# (separator)
NSimul  NClase  NEtapa              ! dimensions
# (separator)
  1  1                              ! ISimul  IClase (per row, NEtapa columns follow)
  ...
```

Maps simulation/hydrology scenario to inflow class per stage — drives stochastic branching in SDDP.

---

#### `plpaflce.dat` — Inflows by plant

```
# (separator)
# (separator)
NCenEmb+NCenSer                    ! number of hydraulic plants
# (separator)
ANGOSTURA                          ! plant name
# (separator)
NClase                             ! number of hydrological classes
# (separator)
  IClase  IBlo  Qaflu              ! class index, block, inflow [m³/s]
  ...
```

These stochastic inflow values are loaded into `EstocRHSP` and used both in the dual LP and to compute the mean inflow for reporting.

---

#### Maintenance files (`plpmance.dat`, `plpmanem.dat`, `plpmanli.dat`, …)

All follow the same pattern as `plpcosce.dat`: count of components, then per component: name, number of entries, then rows of `(NDia, NumBlo, Value)` where `Value` is the maintenance flag or forced outage fraction.

---

### 5.3 Full input file table

| File | Type | Purpose / subsystem |
|---|---|---|
| `plppar.dat` | DAT | Global model parameters (dimensions, horizon) |
| `plppath.dat` | DAT | Path/runtime control file |
| `plprun.dat` | DAT | Run configuration: plane file, iteration range, open mode |
| `plpeta.dat` | DAT | Stage definition: count, duration, discount rate, type label |
| `plpblo.dat` | DAT | Block-to-stage mapping and duration |
| `plpidsim.dat` | DAT | Simulation-to-class index matrix for SDDP sampling |
| `plpidape.dat`, `plpidap2.dat` | DAT | Stage aperture indices |
| `plpbar.dat` | DAT | Bus/node names |
| `plpcnfli.dat` | DAT | Transmission line parameters (capacity, resistance, loss flags) |
| `linconf.csv` | CSV | Auxiliary/alternate line configuration |
| `plpcnfce.dat` | DAT | Plant configuration: all generator types, capacities, efficiencies |
| `centipo.csv` | CSV | Optional plant-type label override for output CSV headers |
| `plpcosce.dat` | DAT | Variable O&M costs by plant and stage |
| `plpdem.dat` | DAT | Demand by bus and block |
| `plpmat.dat` | DAT | SDDP solver control: iterations, tolerances, penalty costs |
| `plpafpar.dat`, `plpaflce.dat` | DAT | Hydrology parameters and inflows per plant/class/stage |
| `plpcenfi.dat`, `plpfilemb.dat` | DAT | Reservoir filter settings (storage volume filtering by plant) |
| `plpvrebemb.dat` | DAT | Spill/rebound reservoir conditions |
| `plpqebnd.dat` | DAT | Reservoir outflow bounds by stage |
| `plpcenre.dat`, `plpcenpmax.dat` | DAT | Piece-wise efficiency and max-power overrides |
| `plpextrac.dat` | DAT | External transfer/extraction configurations |
| `plpmance.dat`, `plpmances.dat` | DAT | Plant maintenance schedules (standard/series) |
| `plpmanem.dat`, `plpmanems.dat`, `plpminembh.dat` | DAT | Reservoir maintenance and minimum storage |
| `plpmanli.dat`, `mantlin.csv`, `mantlins.csv` | DAT/CSV | Line maintenance schedules |
| `plplaja.dat`, `plplajan.dat`, `plplajam.dat` | DAT | Laja irrigation convention inputs |
| `plpmaule.dat`, `plpmaulen.dat` | DAT | Maule irrigation convention inputs |
| `plpriego.dat` | DAT | General irrigation data |
| `plpralco.dat` | DAT | Ralco reservoir constraints |
| `plpplem1.dat`, `plpplem2.dat` | DAT | Initial reservoir storage plan |
| `plpcnfgnl.dat`, `plpcnfgn.dat` | DAT | Natural gas liquids and gas-network restrictions |
| `plpcnfres.dat` | DAT | Frequency reserve service configuration |
| `plpmanresc.dat`, `plpmanresg.dat`, `plpmanresz.dat` | DAT | Reserve maintenance and zone controls |
| `plpcosresc.dat` | DAT | Reserve penalty/scarcity costs |
| `plpcenbat.dat`, `plpmanbat.dat` | DAT | Battery energy-storage modeling and maintenance |
| `plpendes.dat`, `plpendci.dat` | DAT | Legacy compatibility inputs |

> Legacy names `pcp*.dat` and `plphid.dat` appear in comments/compatibility paths and may surface in historical datasets.

---

## 6) Output file reference

### 6.1 General output conventions

- All primary outputs are **comma-separated CSV** files, one per subsystem/component group.
- The first line of each file is the column header row.
- The first three columns are always: `Hidro`, `Bloque`, `TipoEtapa` (hydrology scenario ID, block index, stage-type label from `plpeta.dat`).
- `Hidro` is either a simulation identifier like `Sim  1`, `Sim  2`, … or the literal `MEDIA` for the expected-value (averaged) scenario.
- `Bloque` is the sequential block number (1-based, mapping to stages via `plpblo.dat`).
- Each file is written progressively: simulation `Sim 1` triggers file creation and header; subsequent simulations append rows.
- Output writing is controlled by `FGrabaCSV` in `plpmat.dat` and by optional feature flags.

---

### 6.2 `plpcen.csv` — Generation per plant

```
Hidro,Bloque,TipoEtapa,CenNum,CenNom,CenTip,CenBar,BarNom,
CenQgen,CenPgen,CenEgen,CenInyP,CenInyE,CenRen,CenCVar,CenCostOp,CenPMax
```

| Column | Units | Description |
|---|---|---|
| `Hidro` | — | Simulation ID (`Sim N` or `MEDIA`) |
| `Bloque` | — | Block index |
| `TipoEtapa` | — | Stage-type label (e.g. `HIDRO`, `SECO`) |
| `CenNum` | — | Plant internal sequence number |
| `CenNom` | — | Plant name |
| `CenTip` | — | Plant-type code (`E`=reservoir, `S`=series, `P`=pasada, `T`=thermal, `B`=battery, etc.) |
| `CenBar` | — | Connected bus index |
| `BarNom` | — | Bus name |
| `CenQgen` | m³/s | Turbine discharge flow for hydraulic plants. For thermal/wind/solar plants this slot holds the LP variable value (which maps to power, not a physical water flow) and should be interpreted as a dimensionless dispatch variable |
| `CenPgen` | MW | Active power generation |
| `CenEgen` | GWh | Energy generated in block (`CenPgen × Ren × BloDur × 10⁻³`) |
| `CenInyP` | MW·($/MWh) | Value injection (power × marginal cost) |
| `CenInyE` | k$·h | Energy-weighted injection (energy × CMg × BloDur × 10⁻³) |
| `CenRen` | p.u. | Efficiency/rendering factor |
| `CenCVar` | $/MWh | Variable cost for this stage |
| `CenCostOp` | k$ | Operational cost in block (energy × CVar × BloDur × 10⁻³) |
| `CenPMax` | MW | Maximum generation capacity in block |

---

### 6.3 `plpfal.csv` — Failure generation

Same structure as `plpcen.csv` but contains only failure-generation plants (`CenTip = F`) and only rows where generation > 0. Columns subset:

```
Hidro,Bloque,TipoEtapa,CenNum,CenNom,CenTip,CenBar,BarNom,CenPgen,CenEgen
```

---

### 6.4 `plpcostop.csv` — Total operational cost per block

```
Hidro, IBlo, CostoOperActual
```

| Column | Units | Description |
|---|---|---|
| `Hidro` | — | Simulation ID |
| `IBlo` | — | Block index |
| `CostoOperActual` | k$ | Sum of `CenPGen × CenCVar × BloDur / FPhi` across all plants, where `FPhi` is the per-stage discount factor read from `plpeta.dat` (Tasa column; 1.0 = no discounting) |

---

### 6.5 `plpbar.csv` — Bus/node results (marginal costs)

```
Hidro,Bloque,TipoEtapa,BarNum,BarNom,CMgBar,DemBarP,DemBarE,PerBarP,PerBarE,BarRetP,BarRetE
```

| Column | Units | Description |
|---|---|---|
| `CMgBar` | $/MWh | Marginal cost at this bus, multiplied by the stage discount factor `FPhi` (from `plpeta.dat`) to give the present-value equivalent nodal price |
| `DemBarP` | MW | Bus demand (power) |
| `DemBarE` | GWh | Bus energy demand (`DemBarP × BloDur × 10⁻³`) |
| `PerBarP` | MW | Demand not served (loss-of-load, power) |
| `PerBarE` | GWh | Demand not served (energy) |
| `BarRetP` | k$(MW·h) | Revenue: `DemBarP × CMg` |
| `BarRetE` | k$ | Revenue: `DemBarE × CMg × BloDur × 10⁻³` |

---

### 6.6 `plplin.csv` — Transmission line flows

```
Hidro,Bloque,TipoEtapa,LinNum,LinNom,BarA,BarB,LinFluP,LinFluE,LinFluMax,
LinUso,LinPerP,LinPerE,LinPer2P,LinPer2E,LinITP,LinITE
```

| Column | Units | Description |
|---|---|---|
| `BarA`, `BarB` | — | From-bus and to-bus indices |
| `LinFluP` | MW | Net flow (positive: A→B) |
| `LinFluE` | GWh | Net energy flow (`LinFluP × BloDur × 10⁻³`) |
| `LinFluMax` | MW | Maximum capacity (sum of sub-flows' upper limits) |
| `LinUso` | % | Utilisation rate (`|flow| / FluMax × 100`) |
| `LinPerP` | MW | Losses (power), primary model |
| `LinPerE` | GWh | Losses (energy) |
| `LinPer2P` | MW | Losses (power), alternate model |
| `LinPer2E` | GWh | Losses (energy), alternate model |
| `LinITP` | k$(MW·h) | Implicit transfer pricing (power) |
| `LinITE` | k$ | Implicit transfer pricing (energy) |

---

### 6.7 `plpemb.csv` — Reservoir state

```
Hidro,Bloque,TipoEtapa,EmbNum,EmbNom,EmbFac,EmbVini,EmbVfin,
EmbQgen,EmbQver,EmbQdef,EmbPsom,EmbPsom2,EmbAflu,EmbQFil,EmbQReb
```

| Column | Units | Description |
|---|---|---|
| `EmbFac` | Hm³/internal | Volume scaling factor |
| `EmbVini` | Hm³ | Initial storage in block |
| `EmbVfin` | Hm³ | Final storage in block |
| `EmbQgen` | MW | Hydraulic generation (turbine power) |
| `EmbQver` | m³/s | Spillage |
| `EmbQdef` | m³/s | Deficiency/shortage flow |
| `EmbPsom` | $/MWh | Water shadow price expressed in energy terms: `CMg_water × FPhi × FactTiempo / FactRendim`, where `FactRendim` is the aggregate hydraulic efficiency of the downstream chain. Represents the opportunity cost of storing one unit of water in terms of future energy value |
| `EmbPsom2` | $/Hm³ | Water shadow price in volume terms: `CMg_water × FPhi` directly (without dividing by efficiency) |
| `EmbAflu` | m³/s | Natural inflow (from stochastic RHS) |
| `EmbQFil` | m³/s | Filtered outflow |
| `EmbQReb` | m³/s | Rebound flow |

---

### 6.8 `plpembe.csv` — Extended reservoir (filter/rebound bounds)

```
Hidro,Bloque,TipoEtapa,EmbNum,EmbNom,EmbFac,EmbQFilt,EmbQReb,EmbVRebP,EmbVRebN
```

| Column | Units | Description |
|---|---|---|
| `EmbQFilt` | m³/s | Filtered flow value |
| `EmbQReb` | m³/s | Rebound flow value |
| `EmbVRebP` | Hm³ | Positive rebound storage bound |
| `EmbVRebN` | Hm³ | Negative rebound storage bound |

---

### 6.9 `plpser.csv` — Run-of-river series generation

```
Hidro,Bloque,TipoEtapa,SerNum,SerNom,SerBar,BarNom,
SerQGen,SerQVer,SerPSom,SerPSom2,SerPGen,SerAflu,SerRend
```

| Column | Units | Description |
|---|---|---|
| `SerQGen` | m³/s | Turbine discharge |
| `SerQVer` | m³/s | Spillage |
| `SerPSom` | $/MWh | Water shadow price in energy terms |
| `SerPSom2` | $/Hm³ | Water shadow price in volume terms |
| `SerPGen` | MW | Active power generation |
| `SerAflu` | m³/s | Natural inflow |
| `SerRend` | p.u. | Hydraulic efficiency |

---

### 6.10 `plpextrac.csv` — Extraction/transfers

```
Hidro,Bloque,TipoEtapa, qx<CenInd>@1, qx<CenInd>@2, ...
```

Each additional column is a named extraction variable `qx{CenInd}@{Idx}` where `CenInd` is the plant external ID and `Idx` is the extraction index. Values are in m³/s.

---

### 6.11 `plpbat.csv` — Battery storage

```
Hidro, Bloque, BatNom, SoCf, PGen, EnDes, EnTot, EMin, EMax, PMin, PMax, DualSoC, EnIny1, ...
```

| Column | Units | Description |
|---|---|---|
| `BatNom` | — | Battery name |
| `SoCf` | MWh | State of charge at end of block |
| `PGen` | MW | Net power output (positive = discharge) |
| `EnDes` | MWh | Energy discharged in block |
| `EnTot` | MWh | Total energy throughput in stage |
| `EMin`, `EMax` | MWh | Energy bounds for this block |
| `PMin`, `PMax` | MW | Power bounds |
| `DualSoC` | $/MWh | Shadow price of energy balance (discount-adjusted) |
| `EnIny1`, … | MWh | Energy per injection period (one column per injection slot; `-1` if slot not used by this battery) |

---

### 6.12 `plpcostpres.csv` — Reserve provision costs

```
Hidro, Zona, IBlo, CostoProvActual
```

Sum of `(reserve × cost) × BloDur / FPhi` across all plants and reserve types per zone.

---

### 6.13 `plpres.csv` / `plpresz.csv` — Reserve results

Per-plant (`plpres.csv`) and per-zone (`plpresz.csv`) reserve allocations. Column structure mirrors the reserve service types configured in `plpcnfres.dat` (positive/negative primary/secondary/tertiary reserves by zone).

---

### 6.14 `plpplanos.csv` — Benders cuts (optional)

Contains the SDDP Benders cuts (also called "planes" or "alpha-beta" cuts). Written only when `FGrabaCSV=T` and the plane export is configured. Structure is internal and consumed by subsequent warm-start runs via `plprun.dat`.

---

### 6.15 Log files

| File | Content |
|---|---|
| `plpwarn.log` | Warnings and non-fatal data errors |
| `plpfact.log` | Feasibility-cut diagnostic trace |
| `plpdeb.log` | Debug trace (only when debug mode enabled via `plpdeb.dat`) |

Auxiliary index CSVs written at startup (consumed by post-processing tools):

| File | Description |
|---|---|
| `barras.csv` | Bus name/index mapping |
| `centrales.csv` | Plant name/index mapping |
| `embalses.csv` | Reservoir plant name/index |
| `etapas.csv` | Stage duration/date mapping |
| `lineas.csv` | Line name/index mapping |
| `presuf.csv` | Prefix/suffix labels for simulations |
| `series.csv` | Series plant name/index mapping |
| `simuls.csv` | Simulation label list |
| `costosop.csv` | Cost mapping used by post-processors |

---

## 7) Technical structure

### 7.1 Repository layout

```
plp_cen/
├── PLPv6.3/                  # PLP source code and build system
│   ├── CMakeLists.txt        # Top-level CMake script
│   ├── cmake/                # Custom find-module scripts
│   │   ├── FortranFlags.cmake
│   │   ├── Solver.cmake      # Selects OSI backend at configure time
│   │   ├── FindClp.cmake
│   │   ├── FindOsiClp.cmake
│   │   ├── FindOsiCpx.cmake
│   │   └── FindOsiGrb.cmake
│   └── src/                  # All Fortran, C, and C++ sources
├── CLPv1.16-CDEC/            # Bundled COIN-OR CLP (autotools build)
│   ├── configure
│   ├── Clp/                  # CLP solver
│   ├── Osi/                  # OSI interface (CLP backend)
│   └── CoinUtils/            # COIN utilities
├── OSI-CDECv1.2/             # Bundled OSI for CPLEX/Gurobi
│   └── rev_0_106_20140630/
│       ├── configure
│       ├── Osi/
│       │   └── src/
│       │       ├── OsiCpx/   # CPLEX solver interface
│       │       └── OsiGrb/   # Gurobi solver interface
│       └── CoinUtils/
└── run_plp.sh                # Production run launcher
```

### 7.2 Source file organisation

`PLPv6.3/src/` contains approximately 130 source files. They fall into distinct functional groups:

```
src/
│
├── plp-main.F              ← Program entry point (PROGRAM PLPMAIN)
│
├── ── Input readers ──────────────────────────────────────────────
│   leecnfba.f              Bus/bar configuration
│   leecnfce.f              Plant/generator configuration (largest file)
│   leecnfli.f              Transmission line configuration
│   leedem.f                Demand by bus/block
│   leeeta.f                Stage/time-step definition
│   leeblo.f                Block definition
│   leecosce.f              Variable costs
│   leemat.f                Solver control parameters
│   leeextrac.f             External extractions
│   leefilemb.f             Reservoir filter parameters
│   leevrebemb.f            Rebound conditions
│   leemance.f / leemances.f  Plant maintenance
│   leemanem.f / leemanems.f  Reservoir maintenance
│   leemanli.f / leemanlis.f  Line maintenance
│   plp-leeaflce.f          Inflows per plant
│   plp-leeidsim.f          Simulation index
│   plp-leeidape.f          Aperture indices
│   plp-leecenfi.f          Reservoir filter coupling
│   plp-leecenre.f          Piecewise efficiency curves
│   plp-leecenpmax.f        Max-power overrides
│   leerun.f                Warm-start Benders cut file
│   leecentipo.f            Plant-type CSV override
│   leedebug.f              Debug options
│
├── ── Model formulation ────────────────────────────────────────
│   plpmod.f                PLP module: shared types (PAR_DIMS, etc.)
│   pardims.f               Dimension parameter structure
│   parbaterias.f           Battery parameter structure
│   pargnl.f / pargn.f      GNL/GN restriction parameters
│   parlaja.f / parlajam.f  Laja convention parameters
│   parmaule.f              Maule convention parameters
│   parreserva.f            Reserve service parameters
│   parralco.f              Ralco constraint parameters
│
├── ── LP matrix generation ─────────────────────────────────────
│   defprbpd.f              Define LP problem structure and allocate
│   genmatpd.f / genmatx.f  Base matrix construction
│   genpdcen.f              Generator constraints
│   genpdlin.f              Network (DC power flow) constraints
│   genpdver.f              Spillage constraints
│   genpdago.f / genpdang.f Hydrology balance constraints
│   genpdfil.f              Filter constraints
│   genpdreb.f              Rebound constraints
│   genpdminh.f             Minimum storage constraints
│   genpdqemb.f             Outflow bound constraints
│   genpdextr.f             Extraction constraints
│   genpdlaja.f / genpdlajam.f  Laja irrigation constraints
│   genpdmaule.f            Maule irrigation constraints
│   genpdralco.f            Ralco constraints
│   genpdgnl.f / genpdgn.f  GNL/GN constraints
│   genpdreserva.f          Reserve service constraints
│   genpdbaterias.f         Battery constraints
│   genpdcot.f              Cut (Benders/SDDP plane) constraints
│   pdmatini.f              Matrix initialisation utilities
│   pdmatinv.f / pdmatvar.f Matrix invariant and variable part
│
├── ── SDDP algorithm ────────────────────────────────────────────
│   plp-fasedual.f          Dual (backward) phase: solve LP, add cuts
│   plp-faseprim.f          Primal (forward) simulation phase
│   plp-progdin.f           Dynamic-programming main loop
│   plp-trasdual.f          Dual phase state transitions
│   plp-trasdusi.f          Simulation state transitions
│   plp-agrespd.f           Cut aggregation and selection
│   plp-pdconvrg.f          Convergence test
│   plp-espercnd.f          Expected-cost-to-go calculation
│   addplane.f              Add a Benders cut plane to LP
│   borrespd.f              Remove obsolete cuts
│   pdfin.f / pdinic.f      LP problem finalization / initialization
│   pdlin.f                 LP solve wrapper
│   getdual.f / getprim.f   Extract dual/primal solution vectors
│   amat.f90 / amat.cpp     Sparse matrix utilities
│   plpamat.f               Fortran wrapper for matrix routines
│   salva.f                 State saving/restoring for hot-start
│
├── ── Hydrology models ─────────────────────────────────────────
│   plp-fijamues.f          Fix hydrological sample
│   plp-fijasimu.f          Fix simulation state
│   plp-rendim.f            Compute efficiency from piece-wise curves
│   plp-frendim.f           Filtered efficiency computation
│   plp-filtrac.f / plp-filtracv.f  Volume filter tracing
│   plp-ffiltrac.f          Filtered flow computation
│   plp-fpmaxvol.f          Volume-dependent max-power computation
│   plp-laja0/1/2.f         Laja irrigation convention steps
│   plp-maule0/1/2.f        Maule irrigation convention steps
│   plp-extrprsi.f / plp-extrsimu.f  Extraction state transitions
│   volfinem.f              End-of-horizon volume target
│
├── ── Output writers ────────────────────────────────────────────
│   plp-gradat.f            Main output orchestration (calls GraDat*)
│   plp-gdbdcen.f           Write plpcen.csv and plpfal.csv
│   plp-gdbdbar.f           Write plpbar.csv
│   plp-gdbdlin.f           Write plplin.csv
│   plp-gdbdemb.f           Write plpemb.csv and plpembe.csv
│   plp-gdbdser.f           Write plpser.csv
│   plp-gdbdextr.f          Write plpextrac.csv
│   plp-gdbdcen2.f          Write plpcen.csv (alternate format)
│   plp-gdbdlaj.f / plp-gdbdmau.f  Write Laja/Maule CSVs
│   plp-gdbdple.f / plp-gdbdple2.f  Write plane/cut CSVs
│   genpdreserva.f          (also writes plpres.csv / plpresz.csv)
│   genpdbaterias.f         (also writes plpbat.csv)
│
├── ── Solver interface ──────────────────────────────────────────
│   osicallsc.f90            Fortran module: OSI type bindings (ISO C)
│   osicallsc.hpp            C++ header: extern "C" LP function API
│   osicallsc.cpp            C++ implementation: adapts OSI C++ to C API
│   osisolver.hpp            Solver-selection header (compiled by cmake)
│   iocplex.F                CPLEX-specific preprocessor guards
│
└── ── Utilities ────────────────────────────────────────────────
    plp-version.f            Print version string
    getopts.f                Parse environment-variable runtime flags
    linux-com.f / linux-deb.f  OS-level I/O and debug helpers
    unit.F                   Fortran unit number allocation
    nomcnumc.f / nomcnumf.f  Name-to-index mapping
    num2char.f / wrcharn.f   Number and character formatting helpers
    userstop.f               Check for `userstop` file to abort run
    estadist.f               Statistical utility functions
    tablrel.f / pricas.f     Interpolation and price calculation utilities
    repoprog.f               Progress-reporting helper
```

### 7.3 Program execution flow

```
PLPMAIN (plp-main.F)
│
├─ [Startup]
│   ├─ getopts.f       → read PLP_* environment variables
│   ├─ leemat.f        → read plpmat.dat (solver control)
│   ├─ LeeEtaDim       → read stage count
│   ├─ LeeCnfCenDim    → read plant counts → allocate Dim structure
│   ├─ LeeCnfLinDim    → read line count
│   └─ (allocate all work arrays)
│
├─ [Input reading phase]
│   ├─ LeeEta          → stages, durations, types
│   ├─ LeeBlo          → blocks per stage
│   ├─ LeeCnfBar       → bus names
│   ├─ LeeCnfCen       → all generator data
│   ├─ LeeCnfLin       → network topology
│   ├─ LeeDem          → demand by bus/block
│   ├─ LeeCosCen       → variable costs
│   ├─ LeeIdSim        → scenario/simulation index
│   ├─ LeeIdApe        → aperture indices
│   ├─ LeeAflCen       → stochastic inflows
│   ├─ (optional: maintenance, filters, extractions, …)
│   └─ (optional: Laja/Maule/Ralco/GNL/Reserve/Battery)
│
├─ [LP problem definition]
│   ├─ DefPrbPD        → allocate LP structure
│   ├─ GenMatPD        → build invariant LP matrix (costs, network, …)
│   └─ PDMatIni        → initialise LP
│
├─ [SDDP backward (dual) pass — FaseDual]
│   │   Loops over NEtapa (backward) × NSimul × iterations
│   ├─ For each (stage, simulation, iteration):
│   │   ├─ PDMatVar    → update variable part of LP (RHS, bounds)
│   │   ├─ pdlin.f     → solve LP (via osicallsc.cpp → OSI → solver)
│   │   ├─ getdual.f   → extract dual variables (shadow prices)
│   │   └─ addplane.f  → add Benders optimality cut (plane)
│   └─ plp-pdconvrg.f  → check convergence
│
├─ [SDDP forward (primal) simulation — FasePrim]
│   │   Loops over NEtapa (forward) × NSimul
│   ├─ For each (stage, simulation):
│   │   ├─ PDMatVar    → update LP with current state
│   │   ├─ pdlin.f     → solve LP
│   │   └─ getprim.f   → extract primal solution (generation, flows, …)
│   └─ GraDatS → plp-gradat.f → write all CSV outputs
│
└─ [Shutdown]
    ├─ Write log summaries
    └─ Free all allocations
```

### 7.4 LP problem structure

For each stage–block in the SDDP passes, PLP constructs an LP with:

- **Objective**: minimise total cost = `Σ CenCVar × CenPGen × BloDur`
- **Generation constraints** (per plant): `PMin ≤ PGen ≤ PMax`
- **Spillage/outflow constraints** (hydraulic): `VMin ≤ Q ≤ VMax`
- **Hydraulic balance** (per reservoir): `V(t) = V(t-1) + Qinflow − Qgen − Qspill`
- **DC power flow** (optional): `Σ flows = generation − demand` at each bus
- **Line capacity**: `−TMax ≤ LinFlow ≤ TMax`
- **Reserve service** (optional): allocation constraints per zone and type
- **Battery** (optional): `SoC(t) = SoC(t-1) − energy_discharged + energy_charged`
- **Cost-to-go cut**: the SDDP alpha-beta cut `α + β·V ≥ E[future cost]`

The LP is solved via the Fortran→C→C++ bridge:

```
Fortran (plp-fasedual.f)
  │ osi_lp_loadproblem(...)
  │ osi_lp_dualopt(...)
  │ osi_lp_getrowprice(...)
  ▼
osicallsc.f90 (ISO_C_BINDING)
  ▼
osicallsc.cpp (extern "C" C++ wrapper)
  ▼
OsiSolverInterface (abstract)
  ├── OsiClpSolverInterface   (CLPv1.16-CDEC)
  ├── OsiCpxSolverInterface   (OSI-CDECv1.2 + CPLEX)
  └── OsiGrbSolverInterface   (OSI-CDECv1.2 + Gurobi)
```

### 7.5 Environment variables (set in `run_plp.sh`)

| Variable | Example value | Description |
|---|---|---|
| `PLP_INTERFAZ_MODE` | `no` | Interactive mode: `no`=batch, `si`=ask before continuing |
| `PLP_CONVLAJA_MODE` | `2` | Laja irrigation convention: `0`=off, `1`=classic, `2`=new |
| `PLP_CONVMAULE_MODE` | `2` | Maule irrigation convention |
| `PLP_RESTRALCO_MODE` | `si` | Enable Ralco reservoir constraint |
| `PLP_RENDBDRS_MODE` | `si` | Apply border-adjusted efficiency correction |
| `PLP_SCALE_MODE` | `si` | Enable LP scaling |
| `PLP_SCALEOBJ_MODE` | `1e3` | Objective function scale factor |
| `PLP_SCALEPHI_MODE` | `1e6` | Discount-factor scale |
| `PLP_ANGZERO_MODE` | `si` | Zero reference angle for DC flow |
| `PLP_OPTIEPS_VALUE` | `1e-12` | Optimality cut acceptance tolerance |
| `PLP_OPTIMLD_VALUE` | `1e10` | Optimality cut multiplier limit |
| `PLP_FACTEPS_VALUE` | `1e-10` | Feasibility cut acceptance tolerance |
| `PLP_FACTMLD_VALUE` | `1000` | Feasibility cut multiplier limit |
| `PLP_FACTMXC_VALUE` | `500` | Maximum number of feasibility cuts |
| `PLP_FACTDBL_VALUE` | `1` | Feasibility cut doubling mode |
| `PLP_FACT_MODE` | `3` | Feasibility cut strategy (0=none, 1=primal, 2=farkas, 3=elastic) |
| `PLP_EPOPT_VALUE` | `1e-9` | LP dual-feasibility tolerance |
| `PLP_EPRHS_VALUE` | `1e-9` | LP primal-feasibility tolerance |
| `PLP_AFLUFICT_MODE` | `no` | Use fictitious inflows for infeasibility recovery |
| `PLP_PARAL_MODE` | `si` | Enable OpenMP parallel LP solves |
| `OMP_NUM_THREADS` | (optional) | Number of OpenMP threads |

---

## 8) Roadmap context for future improvements

This documentation is intended to support upcoming engineering sessions focused on:
- adding new operational and market functionalities
- improving I/O throughput and data handling efficiency
- accelerating heavy kernels (including CUDA-capable paths where applicable)
- reducing technical debt in old solver interfaces
- improving portability (including eventual Windows support)
