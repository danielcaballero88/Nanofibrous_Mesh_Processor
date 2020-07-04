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
        e11 = []
        e22 = []
        t11 = []
        t22 = []
        for line in fid:
            vals = [float(val) for val in line.split()]
            e11.append(vals[0] - 1.)
            e22.append(vals[3] - 1.)
            t11.append(vals[4])
            t22.append(vals[7])
    # Graficar
    plt.figure() 
    plt.plot(e11, t11, e11, t22)
    plt.figure()
    plt.plot(e11, e22)
    plt.show()

if __name__ == '__main__':
    main()
