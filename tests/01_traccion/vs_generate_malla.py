import sys 
sys.path.insert(0, '/home/dancab/git/dcVirtualSpinning')
import VirtualSpinning as vs
from pathlib import Path
from matplotlib import pyplot as plt

def main():
    meshParams = {
        'L': 100.,
        'D': 1.,
        'vf': 0.1,
        'ls': 5.,
        'dth': 10. * 3.1416 / 180.,
        'nc': 2,
        'fdo': None,
        'nm': 1
    }
    mc = vs.Mallacom(**meshParams)
    mc.make_malla()
    archivo = Path(__file__).parent / 'malla.txt'
    mc.guardar_en_archivo(archivo)

    # Graficar
    fig, ax = plt.subplots(figsize=(8,6))
    mc.marco.graficar(fig, ax) 
    mc.pre_graficar_fibras(fig, ax, cby='capa')
    plt.show()

if __name__ == '__main__':
    main()
