def sort_and_reset_index(df, column1, column2, ascending=True):
    """
    Esta función ordena un dataframe por dos columnas y reinicia el índice.

    Parámetros:
    df: DataFrame - El dataframe a ordenar.
    column1: str - El nombre de la primera columna por la que se ordenará.
    column2: str - El nombre de la segunda columna por la que se ordenará.
    ascending: bool or list of bools - Indica si el ordenamiento es ascendente o descendente.
                                      Puede ser un único valor booleano o una lista con un valor
                                      booleano para cada columna.

    Devuelve:
    DataFrame - El dataframe ordenado con el índice reiniciado.
    """

    # Ordenar el dataframe
    df_sorted = df.sort_values(by=[column1, column2], ascending=ascending)

    # Reiniciar el índice
    df_sorted.reset_index(drop=True, inplace=True)

    return df_sorted


def XEpsilon(df, stats_list, string_columns, fill_value="No"):
    """
    Esta función realiza múltiples operaciones en un DataFrame, incluyendo agregar columnas de estadísticas máximas y mínimas por equipo,
    rellenar valores faltantes, crear variables dummy basadas en estadísticas, y eliminar columnas específicas.

    Parámetros:
    df: DataFrame - El DataFrame al que se le aplicarán las operaciones.
    stats_list: list - Lista de nombres de columnas de estadísticas para calcular los valores máximos y mínimos por equipo.
    string_columns: list - Lista de nombres de columnas de tipo string para rellenar los NaN.
    fill_value: str, opcional - Valor con el que se rellenarán los NaN en las columnas de tipo string (por defecto es "No").

    Devuelve:
    DataFrame - El DataFrame modificado con las operaciones aplicadas.
    """


    # Agregar columnas de estadísticas máximas y mínimas por equipo
    for stat in stats_list:
        # Nombres de las nuevas columnas para máximos y mínimos
        max_stat_name = stat + '_H'
        min_stat_name = stat + '_L'

        # Calcular máximos y mínimos por equipo
        max_stat = pd.DataFrame({
            "Acronimo": df.groupby(by="Acronimo")[stat].max().index,
            max_stat_name: df.groupby(by="Acronimo")[stat].max().values
        })
        min_stat = pd.DataFrame({
            "Acronimo": df.groupby(by="Acronimo")[stat].min().index,
            min_stat_name: df.groupby(by="Acronimo")[stat].min().values
        })

        # Fusionar los máximos y mínimos con el dataframe original
        df = df.merge(max_stat, on="Acronimo", how="left")
        df = df.merge(min_stat, on="Acronimo", how="left")

    # Rellenar valores faltantes en las columnas especificadas
    df = df["Anio"].drop_duplicates().to_frame().merge(df["Jugador"].drop_duplicates(), how="cross").merge(df, how="left")
    df.reset_index(drop=True, inplace=True)
    df['Posicion'] = df['Posicion'].fillna(method='ffill')

    # Rellenar valores NaN en columnas de tipo string
    for column in string_columns:
        if column in df.columns:
            df[column] = df[column].fillna(fill_value)
        else:
            print(f"La columna '{column}' no existe en el DataFrame.")
            
    # Ordenar los nombres de las columnas alfabéticamente
    sorted_columns = sorted(df.columns)

    # Inicializar las posiciones de inicio y fin
    start = len(sorted_columns)
    end = 0

    # Buscar las posiciones de inicio y fin
    for stat in stats_list:
        max_stat_name = stat + '_H'
        min_stat_name = stat + '_L'

        if max_stat_name in sorted_columns:
            max_stat_position = sorted_columns.index(max_stat_name)
            start = min(start, max_stat_position)

        if min_stat_name in sorted_columns:
            min_stat_position = sorted_columns.index(min_stat_name)
            end = max(end, min_stat_position)

    # Ajustar la posición final para incluir la columna en el rango
    end += 1
    
    high_stat = [stat + '_H' for stat in stats_list]
    low_stat = [stat + '_L' for stat in stats_list]
    
    df[high_stat] = df[high_stat].fillna(method = 'ffill')
    df.fillna(0, inplace = True)
    
    # Hitter
    df_nan = df.isna().any()
    df_name = df.columns
    for con in range(0, len(df_nan)):
        if df_nan[con]:
            print("Name: " + str(df_name[con]))
            
    df['id'] =  df.groupby(['Jugador']).ngroup()
    df.reset_index(drop = True, inplace = True)
    
    # Long of the stats
    end_name = int((len(high_stat) + len(low_stat))/2)
    
    for sport_stat in range(0,end_name):
        I_hitter = []
        for y,max_stat,min_stat in zip(df[stats_list[sport_stat]],
                                       df[high_stat[sport_stat]],
                                       df[low_stat[sport_stat]]):
            # Dummy condition
            if y > (max_stat + min_stat)/2:
                I_hitter.append(0)
            else: 
                I_hitter.append(1)

        I_name = stats_list[sport_stat] + '_I'
        df[I_name] = I_hitter
        
    # Inicializar las posiciones de inicio y fin
    start_I = len(sorted_columns)
    end_I = 0

    # Buscar las posiciones de inicio y fin
    for stat in stats_list:
        max_I_name = stat + '_I'
        min_I_name = stat + '_I'

        if max_stat_name in sorted_columns:
            max_stat_position = sorted_columns.index(max_I_name)
            start_I = min(start_I, max_stat_position)

        if min_stat_name in sorted_columns:
            min_stat_position = sorted_columns.index(min_I_name)
            end_I = max(end_I, min_stat_position)

    # Ajustar la posición final para incluir la columna en el rango
    end += 1
    
    # Good practice
    high_stat = sorted(high_stat)
    # Dummy
    I_df = sorted([stat_I + '_I' for stat_I in stat_list])
    
    # Pitcher
    for stat in range(0,len(stat_list)):
        # Variable auxiliar
        X_df = []

        # Variables 
        i = (-1)**(df[I_hitter[stat]])
        x = df[stat_list[stat]]/np.sqrt(df[high_stat[stat]])
        X_df = i*x

        # X name
        name = 'X_' + stat_list[stat]
        df[name] = X_df
        
    # define a list of suffixes to drop
    drop_suffix = ['_I', '_L', '_H']

    # Use a list comprehension to filter the columns to drop
    cols_to_drop = [col for col in df.columns if col.endswith(tuple(drop_suffix))]

    # Drop the selected columns
    df = df.drop(cols_to_drop,
                 axis=1)

    return df


def Y_var(df, column_name):
    """
    Calcula las diferencias de las raíces cuadradas de los valores consecutivos en una columna específica para cada identificador único en el DataFrame.

    Parámetros:
    df: DataFrame - El DataFrame que contiene los datos.
    column_name: str - El nombre de la columna en la que se realizará el cálculo.

    Devuelve:
    DataFrame - El DataFrame original con una nueva columna 'Y' que contiene las diferencias calculadas.
    """

    def sqrt_dif(X):
        """Calcula la diferencia de las raíces cuadradas de valores consecutivos en una lista."""
        S = [np.sqrt(X[i+1]) - np.sqrt(X[i]) for i in range(len(X) - 1)]
        S.append(S[-1] if S else 0)  # Repite el último valor o añade 0 si la lista está vacía
        return S

    Y_values = []
    for p in df["id"].unique():
        # Filtrar los valores de la columna especificada para cada identificador único
        X = df[df["id"] == p][column_name].values
        # Aplicar la función sqrt_dif y añadir los resultados
        Y_values.extend(sqrt_dif(X))

    # Agregar la nueva columna al DataFrame
    df["Y"] = Y_values
    return df
