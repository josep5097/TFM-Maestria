import numpy as np
import pandas as pd

ventana = 2

numeros = ['1','2',
           '3','4',
           '5','6',
           '7','8',
           '9','10']

def dividir_serie(serie, ventana, paso):
        X = []
        Y = []
        
        for i in range(0, len(serie) - ventana):

            if (i+ventana+paso) > len(serie)-1:
                break
            
            ventana_actual = serie[i:i+ventana]
            siguiente_valor = serie[i+ventana+paso]
            
            X.append(ventana_actual)
            Y.append(siguiente_valor)
        
        X = np.array(X)
        Y = np.array(Y)
        
        return X, Y

for numero in numeros:
    serie = pd.read_excel('FinalSamples_ID_Sim2.xlsx',numero, engine='openpyxl')
    serie = serie['Measurement'].tolist()

    # 15 Minutos
    paso = 0
    X, Y = dividir_serie(serie, ventana, paso)
    print("Matriz X con paso 1:")
    print(X)
    print("Matriz Y con paso 1:")
    print(Y)
    df_x = pd.DataFrame(X, columns = ['X1','X2'])
    df_y = pd.DataFrame(Y, columns = ['Y1'])
    paso_0 = pd.concat([df_x, df_y], axis=1)

    nombre = 'Ventana2/Sim/Ventana15_' + numero + '.xlsx'
    paso_0.to_excel(nombre, sheet_name=numero)

    # 30 Minutos
    paso = 1
    X, Y = dividir_serie(serie[:-1], ventana, paso)
    print("Matriz X con paso 2:")
    print(X)
    print("Matriz Y con paso 2:")
    print(Y)

    df_x = pd.DataFrame(X, columns = ['X1','X2'])
    df_y = pd.DataFrame(Y, columns = ['Y1'])
    paso_1 = pd.concat([df_x, df_y], axis=1)

    nombre = 'Ventana2/Sim/Ventana30_' + numero + '.xlsx'
    paso_1.to_excel(nombre, sheet_name=numero)
