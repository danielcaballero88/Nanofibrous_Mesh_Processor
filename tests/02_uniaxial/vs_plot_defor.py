import sys 
sys.path.insert(0, '/home/dancab/git/dcVirtualSpinning')
import VirtualSpinning as vs
from pathlib import Path
from matplotlib import pyplot as plt

LIM = 70
LIMITES = {'bottom': -LIM, 'right': LIM, 'top':LIM, 'left': -LIM}

def main():
    for im in range(1,17,1):
        # Leer
        archname = f'malla_i_s_uaxi_{im:04d}.txt'
        archivo = Path(__file__).parent / archname
        ms = vs.Mallasim.leer_de_archivo(archivo)
        # Preparo para graficar por variable tension (aproximada) 
        lamefs = ms.fibras.calcular_lamefs(ms.nodos.r)
        mask = lamefs > 1. 
        tenefs = 2.9*(lamefs -1.)
        tenefs[mask] = 1000.*tenefs[mask]
        # Graficar
        fig, ax = plt.subplots(figsize=(8,6))
        ms.pre_graficar_bordes(fig, ax, limites=LIMITES)
        ms.pre_graficar(fig, ax, cby=tenefs,
                        cbar=True, cmap="rainbow", cvmin=-5., cvmax=80.,
                        plot_afin=True,
                        plot_enrul=True, 
                        plot_broken=True)
        fig.savefig(archname[:-4] + '.png')
    # plt.show()


if __name__ == '__main__':
    main()
