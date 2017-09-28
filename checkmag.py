#! /usr/bin/env python3

from pymatgen.io.vasp.outputs import Outcar
from math import fabs
import sys
import argparse

parser = argparse.ArgumentParser()
parser.description='Report ion magnetization data from a VASP OUTCAR file'
parser.add_argument( '-o', '--outcar', help='OUTCAR file to read from', default='OUTCAR' )
parser.add_argument( '-t', '--threshold', help='only report magnetic moments larger than this threshold.', default=0.5, type=float )

if __name__ == '__main__':
    args = parser.parse_args()
    try:
        outcar = Outcar( args.outcar )
    except:
        raise
    selected_atoms = [ [ i, m ] for i, m in enumerate( outcar.magnetization ) if fabs( m['tot'] ) > args.threshold ]
    for a in selected_atoms:
        print( a[0]+1, ' '.join( [ "{}: {}".format( k, a[1][k] ) for k in a[1] ] ) )
