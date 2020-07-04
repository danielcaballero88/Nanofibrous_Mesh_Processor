import sys 
sys.path.insert(0, '/home/dancab/git/dcVirtualSpinning')
import VirtualSpinning as vs
from pathlib import Path
from matplotlib import pyplot as plt


def main():
    # Leer malla inicial
    archivo = Path(__file__).parent / 'malla.txt'
    mc = vs.Mallacom.leer_de_archivo(archivo)
    # Leer malla intersectada
    archivo = Path(__file__).parent / 'malla_i.txt'
    mc_i = vs.Mallacom.leer_de_archivo(archivo)
    # Leer malla intersectada y simplificada
    archivo = Path(__file__).parent / 'malla_i_s.txt'
    mc_i_s = vs.Mallasim.leer_de_archivo(archivo)        

    # Graficar malla inicial
    fig, ax = plt.subplots(figsize=(8,6))
    mc.marco.graficar(fig, ax) 
    mc.pre_graficar_fibras(fig, ax, cby='capa')
    fig.savefig('malla.png')

    # Graficar malla intersectada
    fig, ax = plt.subplots(figsize=(8,6))
    mc_i.marco.graficar(fig, ax) 
    mc_i.pre_graficar_interfibras(fig, ax, color_por='lamr')
    fig.savefig('malla_i.png')

    # Graficar malla simplificada
    fig, ax = plt.subplots(figsize=(8,6))
    mc_i_s.pre_graficar_bordes(fig, ax)
    mc_i_s.pre_graficar(fig, ax, cby="lamr",
                    cbar=True, cmap="rainbow",
                    plot_afin=False)
    fig.savefig('malla_i_s.png')

    plt.show()


if __name__ == '__main__':
    main()
