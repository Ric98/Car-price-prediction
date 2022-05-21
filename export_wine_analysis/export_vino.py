import numpy as np
import matplotlib.pyplot as plt
import pandas as pd
from scipy.optimize import curve_fit
import math
import scipy.optimize as optim
from scipy.integrate import odeint
import numpy as np
import pandas as pd
df = pd.read_excel(r'C:\Users\feder\PycharmProjects\pythonProject\strutturati\cluster_codice2.xlsx')
#Tengo solo gli anni per indicare il periodo
for i in range(len(df)):
    df.iat[i,1] = int(df.iat[i,1].split('.')[2].replace(" ",""))
#Modifico il nome di alcuni paesi
cluster = df.iloc[:,0].unique()
for i in range(len(df)):
    if df.iat[i,0] == 'Germany (incl. German Democratic Republic \'DD\' from 1991)':
        df.iat[i,0] = 'Germany'
    else:
        continue
for i in range(len(df)):
    if df.iat[i,0] == 'Spain (incl. Canary Islands \'XB\' from 1997)':
        df.iat[i,0] = 'Spain'
    else:
        continue
for i in range(len(df)):
    if df.iat[i,0] == 'France (incl. Saint Barthélemy \'BL\' -> 2012; incl. French Guiana \'GF\', Guadeloupe \'GP\', Martinique \'MQ\', Réunion \'RE\' from 1997; incl. Mayotte \'YT\' from 2014)':
        df.iat[i,0] = 'France'
    else:
        continue
for i in range(len(df)):
    if df.iat[i,0] == 'Belgium (incl. Luxembourg \'LU\' -> 1998)':
        df.iat[i,0] = 'Belgium'
    else:
        continue
for i in range(len(df)):
    if df.iat[i,0] == 'Intra-EU27 (AT, BE, BG, CY, CZ, DE, DK, EE, ES, FI, FR, GR, HR, HU, IE, IT, LT, LU, LV, MT, NL, PL, PT, RO, SE, SI, SK, QR, QV, QY)':
        df.iat[i,0] = 'Intra-EU27'
    else:
        continue
df_cumulativo = pd.DataFrame(columns =['VALUE_IN_EUROS', 'PARTNER','PERIOD/INDICATORS'])
df_futuro = pd.DataFrame(columns=['PERIOD/INDICATORS','PARTNER','VALUE'])
def logistic_de(t, N, r, K):
    return r*N*(1 - N/K)
# include N0 as an argument
def logistic_solution(t, N0, r, K):
    return odeint(logistic_de, N0, t, (r, K), tfirst=True).ravel()
df_cleaned = df.drop(df[df.iloc[:,2].isna()].index)
ind = np.arange(len(cluster))
errori = pd.DataFrame(columns = ['Cluster', 'RMSE','N0', 'k', 'r', 'livello_saturazione'])
errore_parziale = pd.DataFrame(columns = ['Cluster', 'RMSE', 'N0', 'k', 'r', 'livello_saturazione'], index = np.arange(len(cluster)))
for j in range(len(cluster)):
    errore_parziale['Cluster'][j] = cluster[j]
    df_paesi = df_cleaned.loc[df_cleaned['PARTNER']==cluster[j],:]
    ydata = np.asarray(df_paesi.iloc[:,2].cumsum())
    value = pd.DataFrame(ydata, columns =['VALUE_IN_EUROS']).reset_index()
    value['PARTNER'] = cluster[j]
    anni = pd.DataFrame(np.arange(min(df_paesi.iloc[:,1]), max(df_paesi.iloc[:,1])), columns = ['PERIOD/INDICATORS'])
    value['PERIOD/INDICATORS'] = anni
    df_cumulativo=pd.concat([df_cumulativo, value], axis = 0, ignore_index = True)
    xdata = range(len(ydata))
    xdata = np.asarray(xdata)
    n_years = max(df_paesi.iloc[:,1]) - (min(df_paesi.iloc[:,1]))
    x_labs = np.arange(min(df_paesi.iloc[:,1]), 2036)
    anni_futuro = pd.DataFrame(x_labs, columns =['PERIOD/INDICATORS'])
    anni_futuro['PARTNER'] = cluster[j]
    n_years_forward = 2036 - (min(df_paesi.iloc[:,1]))
        #plt.plot(xdata, ydata, 'o')
    N0 = ydata[0]
    parsic = [0.25, ydata[len(ydata)-1]]
    # N0 thus included as parameter to fit
    params, _ = optim.curve_fit(logistic_solution, xdata, ydata,
                                p0=[N0, *parsic])
    errore_parziale['N0'][j]=params[0]
    errore_parziale['k'][j] = params[2]
    errore_parziale['r'][j] = params[1]
    # N1 integral factors in the fitted N0 parameter
    # (not the same as the global variable named N0,
    # should change global variable to something like N0_guess)
    N2 = odeint(logistic_de, params[0], np.linspace(0, n_years_forward, n_years_forward+1),
                tuple(params[1:]), tfirst=True)
    N1 = odeint(logistic_de, params[0], np.linspace(0, n_years_forward, 100000),
                tuple(params[1:]), tfirst=True)
    x_ticks = x_labs.tolist()
    y_ticks = ydata.tolist()
    plt.figure(figsize = (20,15))
    plt.plot(np.linspace(0, n_years_forward, 100000), N1)
    plt.scatter(xdata, ydata)
    plt.ylabel("Export value", fontsize=30)
    plt.title("Export Forecast Value "+ cluster[j], fontsize = 35)
    plt.yticks(np.linspace(min(ydata),N1[len(N1)-1],10), fontsize = 25)
    plt.xticks(range(0,n_years_forward), labels = x_ticks, rotation = 60, fontsize = 25)
    errore = 0
    for k in range(len(ydata)):
         errore += (ydata[k]-N2[k])**2
         errore = (errore/len(ydata))**0.5
    errore_parziale.iat[j,1] = errore[0]
    plt.locator_params(axis='both', nbins=10)
    name_fig = 'previsione'+cluster[j]+'.jpg'
    plt.savefig(name_fig,bbox_inches='tight')
    errore_parziale['livello_saturazione'][j] = max(N1)
errori = pd.concat([errori, errore_parziale])
errori.reset_index(drop=True, inplace=True)

cluster_2 = cluster[1:3]
for j in range(len(cluster_2)):
    errore_parziale['Cluster'][j] = cluster_2[j]
    df_paesi = df_cleaned.loc[df_cleaned['PARTNER']==cluster_2[j],:]
    ydata = np.asarray(df_paesi.iloc[:,2].cumsum())[7:]
    value = pd.DataFrame(ydata, columns =['VALUE_IN_EUROS']).reset_index()
    value['PARTNER'] = cluster_2[j]
    anni = pd.DataFrame(np.arange(2009, max(df_paesi.iloc[:,1])), columns = ['PERIOD/INDICATORS'])
    value['PERIOD/INDICATORS'] = anni
    df_cumulativo=pd.concat([df_cumulativo, value], axis = 0, ignore_index = True)
    xdata = range(len(ydata))
    xdata = np.asarray(xdata)
    n_years = max(df_paesi.iloc[:,1]) - 2009
    x_labs = np.arange(2009, 2036)
    anni_futuro = pd.DataFrame(x_labs, columns =['PERIOD/INDICATORS'])
    anni_futuro['PARTNER'] = cluster_2[j]
    n_years_forward = 2036 - 2009
        #plt.plot(xdata, ydata, 'o')
    N0 = ydata[0]
    parsic = [5, ydata[len(ydata)-1]]
    # N0 thus included as parameter to fit
    params, _ = optim.curve_fit(logistic_solution, xdata, ydata,
                                p0=[N0, *parsic])
    errore_parziale['N0'][j]=params[0]
    errore_parziale['k'][j] = params[2]
    errore_parziale['r'][j] = params[1]
    # N1 integral factors in the fitted N0 parameter
    # (not the same as the global variable named N0,
    # should change global variable to something like N0_guess)
    N2 = odeint(logistic_de, params[0], np.linspace(0, n_years_forward, n_years_forward+1),
                tuple(params[1:]), tfirst=True)
    N1 = odeint(logistic_de, params[0], np.linspace(0, n_years_forward, 100000),
                tuple(params[1:]), tfirst=True)
    x_ticks = x_labs.tolist()
    y_ticks = ydata.tolist()
    plt.figure(figsize = (20,15))
    plt.plot(np.linspace(0, n_years_forward, 100000), N1)
    plt.scatter(xdata, ydata)
    plt.ylabel("Export value", fontsize=30)
    plt.title("Export Forecast Value "+ cluster_2[j], fontsize = 35)
    plt.yticks(np.linspace(min(ydata),N1[len(N1)-1],10), fontsize = 25)
    plt.xticks(range(0,n_years_forward), labels = x_ticks, rotation = 60, fontsize = 25)
    errore = 0
    for k in range(len(ydata)):
         errore += (ydata[k]-N2[k])**2
         errore = (errore/len(ydata))**0.5
    errore_parziale.iat[j,1] = errore[0]
    plt.locator_params(axis='both', nbins=10)
    name_fig = 'previsione'+cluster_2[j]+'.jpg'
    plt.savefig(name_fig,bbox_inches='tight')
errori = pd.concat([errori, errore_parziale])
errori.reset_index(drop=True, inplace=True)