import numpy as np
import math
from matplotlib import pyplot as plt


class Fibra(object):
    """
    Clase para calcular la curva de una sola fibra traccionada
    """
    def __init__(self, Et, EbEt, doteps, s0, nh, lamr=1., tenbrk=1000.):
        self.param = {
            'Et' : Et,
            'Eb' : EbEt * Et,
            'doteps' : doteps,
            's0' : s0,
            'nh' : nh,
            'tenbrk': tenbrk
        }
        self.broken = False
        self.lamr = lamr
        self.lamp = 1.

    def calc_ten(self, lam):
        """ 
        Calcula la tension en un incremento de tiempo
        puede haber plasticidad y puede estar rota
        pero aca no se incrementan esas variables
        """
        # Tomo algunas variables mas comodas
        Et = self.param['Et']
        Eb = self.param['Eb']
        lamrp = self.lamr * self.lamp
        # Calculo segun el caso
        if self.broken:  # fibra rota
            ten = 0.
        elif lam < lamrp:  # fibra enrulada
            ten = Eb * (lam - 1.)
        else:  # fibra reclutada
            tenr = Eb*(lamrp - 1.)  # tension en el punto de reclutamiento
            ten = tenr + Et * (lam / lamrp - 1.)
        return ten

    def calc_plas(self, ten, dt):
        """
        Calcula la tasa de deformacion plastica en funcion de la tension
        Tambien la rotura si se produce
        """

        # Calculo la tasa de plasticidad y/o si rompe la fibra
        if self.broken:
            dotlamp = 0.
        elif ten > self.param['tenbrk']:
            self.broken = True 
            self.dotlamp = 0. 
        else: 
            s = self.param['s0'] * self.lamp**self.param['nh']
            dotlamp = self.param['doteps'] * math.sinh(ten / s)

        # Incremento la plasticidad
        self.lamp = self.lamp + dotlamp * dt

    def traccionar(self, dt, dotlam, lamf):
        """
        Calcular la curva tension vs lam para una traccion en el tiempo
        """
        # Variables iniciales del esquema temporal
        time = 0.
        lam = 1.
        # Listas de variables principales a guardar
        rec_time = [time]
        rec_lam = [lam]
        rec_ten = [0.]
        # Lista de otras variables a guardar
        rec_lamp = [self.lamp] 
        rec_lamef = [1. / self.lamr / self.lamp]
        while lam < lamf:
            time += dt 
            lam += dotlam * dt
            ten = self.calc_ten(lam)
            self.calc_plas(ten, dt)
            rec_time.append(time) 
            rec_lam.append(lam) 
            rec_ten.append(ten)
            rec_lamp.append(self.lamp) 
            rec_lamef.append(lam / self.lamr / self.lamp)
        rec = {
            'time': rec_time,
            'lam': rec_lam, 
            'ten': rec_ten,
            'lamp': rec_lamp, 
            'lam_ef': rec_lamef
        }
        return rec

def main():
    parcon = {
        'Et':2.9e3,
        'EbEt': 1.0e-3,
        'doteps': 1.0e-8,
        's0': 4.5,
        'nh': 1.0,
        'tenbrk': 100.0
    }

    fibra = Fibra(**parcon, lamr=1.1)
    dt = 0.01
    lamf = 1.3
    dotlam = 0.01
    rec = fibra.traccionar(dt, dotlam, lamf)

    lam_ef = np.array(rec['lam_ef'])
    mask = lam_ef > 1 
    ten_ef = 2.9*(lam_ef - 1.)
    ten_ef[mask] = ten_ef[mask]*1000.

    plt.figure()
    plt.plot(rec['lam'], rec['ten'])
    plt.plot(rec['lam'], ten_ef)

    plt.figure()
    plt.plot(rec['lam'], rec['lam_ef'])

    plt.figure() 
    plt.plot(rec['lam_ef'], rec['ten'])
    
    plt.show()

if __name__ == '__main__':
    main()