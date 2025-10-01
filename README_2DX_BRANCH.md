# Focus 2DX Branch - 2D Electron Crystallography Only

This branch has been specifically created to support only **2D Electron Crystallography** processing, removing all components and dependencies related to other project modes.

## What was removed:

### Project Modes
- Drift Correction Only (mode 0)
- Single Particle (mode 2) 
- Electron Tomography (mode 3)
- Multi Exposure (mode 4)
- Single Particle with Xstal Filter (mode 5)
- Ptycho (mode 6)

### Removed Components:

#### Scripts and Processing:
- `scripts/singleparticle/` - All single particle processing scripts
- `scripts/image/tomoimport/` - Tomography import scripts
- `scripts/image/tomoalign/` - Tomography alignment scripts
- `scripts/image/ptychoimport/` - Ptychography import scripts
- `scripts/image/particles/` - Particle processing scripts
- `scripts/image/xfilter/` - Crystal filter scripts for single particle

#### Source Code:
- `kernel/mrc/source/2dx_single_particle_lib/` - Entire single particle library (289 files)
- `apps/stale/eulerWindow.*` - Single particle GUI components
- `apps/stale/reprojectWindow.*` - Single particle GUI components

#### Configuration:
- Removed EMAN2, FREALIGN, RELION directory references (used primarily for single particle)
- Filtered `2dx_master.cfg` to remove parameters not applicable to 2D Electron Crystallography
- Kept IMOD references as they are needed for drift correction in 2DX

#### Code Simplifications:
- Modified `ProjectMode::availableModes()` to only return 2D Electron Crystallography
- Simplified project mode checks in `ExecutionWindow.cpp` and `AutoImportWindow.cpp`
- Removed Multi Exposure mode specific parameter loading

## What was preserved:

### Core 2D Crystallography Components:
- `scripts/image/2DXtal/` - All 2D crystallography specific scripts
- `scripts/image/align/` - Drift correction and alignment (used by 2DX)
- `scripts/image/import/` - Image import functionality
- `scripts/image/process/` - Image processing scripts
- `scripts/merge/` - Merging functionality
- `scripts/diffmaps/` - Difference map analysis
- All FORTRAN executables for 2D crystallography processing
- IMOD integration (used for drift correction)

### Result:
- Reduced codebase by 691 files and ~420,000 lines of code
- Simplified GUI with only 2D Electron Crystallography mode available
- Maintained all functionality needed for 2D crystallography processing
- Removed dependencies on EMAN2, FREALIGN, and RELION

## Usage:
This branch provides a streamlined version of Focus specifically for 2D Electron Crystallography research groups who don't need the complexity of multi-mode support.