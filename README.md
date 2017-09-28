# VASP Utilities

Shell scripts and other code for helping with processing VASP input / output.

Repository contents:

* [checkforce](#checkforce)
* [checkmag](#checkmag)
* [checkmurn](#checkmurn)
* [get_potcar_list](#get_potcar_list)
* [mkmurn](#mkmurn)
* [mkpotcar](#mkpotcar)
* [mktrajectory](#mktrajectory)
* [murncalc](#murncalc)
* [parseDOSCARf.f90](#parseDOSCARf)
* [README](#readme)

## checkforce

Looks for OUTCAR in the current directory, and checks the most recently outputted set of forces against the convergence criterion.

For options use `checkforce -h`:

    List force convergence from VASP OUTCAR

    Optional arguments:
      -h, --help                  print this message

      -c, --convergence=CONV      set force convergence to CONV [this will override --outcar]
      -o, --outcar                read force convergence from OUTCAR 
      -v, --verbose               show convergence status for all atoms

    Standard output:
    remainder: < sum{force on atom - force convergence} / number of atoms >
    maximum: < max non-converged forces >
    not-opt: < number of non-converged forces > / < number of atoms >
    
## checkmag

```
usage: checkmag.py [-h] [-o OUTCAR] [-t THRESHOLD]

optional arguments:
  -h, --help            show this help message and exit
  -o OUTCAR, --outcar OUTCAR
                        OUTCAR file to read from
  -t THRESHOLD, --threshold THRESHOLD
                        only report magnetic moments larger than this
                        threshold.
```
	
Python dependencies: `pymatgen`
	
## checkmurn

Checks convergence for calculations running for a Murnaghan equation of state fit. Each subdirectory that begins with a numeral is treated as a lattice parameter label, and [checkforce](#checkforce) is called.

## get_potcar_list.py

Utility script for listing all VASP pseudopotential subdirectories. The root directory for the pseudopotential set should be set in the `POTCARDIR` environment variable, e.g. in your `.bashrc`

```
export POTCARDIR=the/path/to/your/pseudopotentials/PBE54
```

```
usage: get_potcar_list.py [-h] [-l] [-c COLUMNS]

optional arguments:
  -h, --help            show this help message and exit
  -l, --list            Print the list of pseudopotentials as a formatted
                        list.
  -c COLUMNS, --columns COLUMNS
                        Set the number of columns for list output
```

If the `-l` flag is set without `-c` to set the number of columns, the output will automatically fit to the current terminal window.

## mkmurn

**deprecated**  
A complete Murnaghan equation of state fitting procedure can be found at https://github.com/bjmorgan/vasppy/blob/master/scripts/murnfit.py (part of [`vasppy`](https://github.com/bjmorgan/vasppy)).

## mkpotcar

Requires `POTCARDIR` set as an environment variable.
Syntax is `mkpotcar <P1> <P2> ...`, where `P1` and `P2` (etc.) are names of pseudo potential directories in `POTCARDIR`.
If any of the requested directories are not found in `POTCARDIR` then [get_potcar_list.py](#get_potcar_list) is called with the `-l` flag, to give a listing of the available pseudopotentials.
If `get_potcar_list.py` is in your path, then you can enable tab completion for valid pseudopotential directories by adding the following to your `.bashrc`:
```bash
_mkpotcar()
{
  _potcars=$(get_potcar_list.py)
  local cur
  COMPREPLY=()
  cur="${COMP_WORDS[COMP_CWORD]}"
  COMPREPLY=( $(compgen -W "${_potcars}" -- ${cur}) )

  return 0
}
complete -o nospace -F _mkpotcar mkpotcar
```

## mktrajectory

Generates a `.xyz` formatted trajectory file from the sequence of ionic positions saved in OUTCAR. Requires POSCAR to read in the numbers of each ionic species.

## murncalc

**deprecated**  
A complete Murnaghan equation of state fitting procedure can be found at https://github.com/bjmorgan/vasppy/blob/master/scripts/murnfit.py (part of [`vasppy`](https://github.com/bjmorgan/vasppy)).

## parseDOSCARf

`parsedoscar` is now a separate repository at https://github.com/bjmorgan/parsedoscar.

## README.md

This file.
