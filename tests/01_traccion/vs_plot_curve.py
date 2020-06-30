import sys 
sys.path.insert(0, '/home/dancab/git/dcVirtualSpinning')
import VirtualSpinning as vs
from pathlib import Path
from matplotlib import pyplot as plt

LIMITES = {'bottom': -65, 'right': 65, 'top':65, 'left': -65}

def main():
    # Leer
    archname = f'malla_c.txt'
    archivo = Path(__file__).parent / archname
    with open(archivo, 'r') as fid:
        lams = []
        tens = []
        for line in fid:
            vals = [float(val) for val in line.split()]
            lams.append(vals[0])
            tens.append(vals[4])
    # Graficar
    plt.figure() 
    plt.plot(lams, tens)
    plt.show()

if __name__ == '__main__':
    main()
