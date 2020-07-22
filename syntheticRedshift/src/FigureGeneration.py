import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import StarFormationRate
import os

z=np.linspace(0.1,100,1000)

model_names=['H06'                  # model names being use
            ,'L08'
            ,'B10'
            ,'M14'
            ,'M17'
            ,'F18'
            ,'P15'
            ]

names_of_txt_files = [ 'LogRate' + model_names for model_names in model_names ]

out_dir = '../out'

folder_destination_to_read=[ out_dir +'/' + names +'/' + names + '_process_1_sample.txt' for names in names_of_txt_files ]

sfr_functions = [ getattr(StarFormationRate,'get' + model) for model in names_of_txt_files ] #calls functions within StarFormationRate given model names


for i , model in enumerate(model_names):
    df = pd.read_csv(folder_destination_to_read[i])
    df.hist('SampleVariable1',bins=len(z),alpha=0.25,density=True, label='Denisty histogram')
    sfr = np.exp(sfr_functions[i](z))
    plt.plot( z , sfr / max(sfr) , label = 'sfr function')
    plt.title(model)
    plt.xlabel('redshift(z)')
    plt.xscale('log')
    plt.legend()
    fig_dir=out_dir+'/fig'
    if not os.path.isdir(fig_dir): os.mkdir(fig_dir)
    plt.savefig(os.path.join(fig_dir , model))

    #plt.show()