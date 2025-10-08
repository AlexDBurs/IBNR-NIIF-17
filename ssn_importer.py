import pandas as pd
import warnings
import chainladder as cl  # Sparse=0.14.0 ; incremental.py np.NINF -> -np.inf
import argparse

warnings.filterwarnings("ignore")


def claims_formatter(doc: pd.DataFrame):
    doc_copy = doc.copy()

    # Acomodo indice
    doc_copy.index = doc_copy.AUTOMOTORES
    formatted_doc = (
        doc_copy.iloc[:, 4:]
        .stack(level=[0, 1, 2, 3])
        .reset_index([1, 2, 3, 4])
        .drop_duplicates()
    )
    formatted_doc.reset_index(inplace=True)
    formatted_doc[["Base Origen", "Ramo", "Codigo 1", "Codigo 2"]] = pd.DataFrame(
        formatted_doc[("index", "")].to_list(), index=formatted_doc.index
    )

    # Acomodo columnas
    formatted_doc.columns = [
        "indice",
        "Tipo",
        "Periodo",
        "Desarrollo 1",
        "Año Valuacion",
        "Pendientes",
        "Pagados",
        "Incurridos",
        "Cantidad",
        "Base Origen",
        "Ramo",
        "Codigo 1",
        "Codigo 2",
    ]
    formatted_doc["Año Ocurrencia"] = pd.to_datetime(
        formatted_doc["Periodo"][0][0:5]
    ) + pd.DateOffset(months=6)
    formatted_doc["Año Valuacion"] = pd.to_datetime(formatted_doc["Año Valuacion"])
    formatted_doc.drop(
        ["indice", "Tipo", "Periodo", "Desarrollo 1"], axis=1, inplace=True
    )

    return formatted_doc

#Parsear argumentos
parser = argparse.ArgumentParser(description="Parseador de Excel de SSN a CSV con FACPCE aplicado  al a ultima fecha del triángulo.")
parser.add_argument("tri_input", type = str, help = "Ruta del Excel del triángulo de la SSN.")
parser.add_argument("--tri_facpce", type = str, help = "Rura del Excel con el índice de FACPCE.",
                     default = ".\\sample\\Indice-FACPCE-Res.-JG-539-18-_2025-07-1.xlsx")
args = parser.parse_args()

#Importar archivo
sto_input = pd.read_excel(
    args.tri_input,
    sheet_name=None,
    header=[0, 1, 2, 3, 4, 5],
)
facpce_input = pd.read_excel(
    args.tri_facpce, skiprows=2, skipfooter=4
)

# Tratamiendo desarrollo de stos
# sto_input = dict(itertools.islice(sto_input.items(), 3, len(sto_input)))
sto_input_formateado = [claims_formatter(sto_input[k]) for k in sto_input.keys() if (k[0] == "2") or (k[0] == "1")]
db_stos = pd.concat(sto_input_formateado)

# Tratamiento indice facpce
db_facpce = facpce_input.set_index("MES")
factores_facpce = (
    db_facpce.loc[max(db_stos["Año Valuacion"]) - pd.DateOffset(days=29)]
    / db_facpce.loc[db_stos["Año Valuacion"] - pd.DateOffset(months=5, days=29)]
)  # Asumo que esta todo valuado a mitad de periodo.
db_stos["Factor FACPSE"] = factores_facpce.set_index(db_stos.index)

# Armo el dataframe para el triángulo que quiero analizar
idx = db_stos["Base Origen"] == "[HG]"
db_triangle = db_stos.loc[
    idx,
    [
        "Año Ocurrencia",
        "Año Valuacion",
        "Pendientes",
        "Pagados",
        "Incurridos",
        "Factor FACPSE",
        "Cantidad",
        "Ramo",
    ],
]
db_triangle["Año Valuacion"] = db_triangle["Año Valuacion"] + pd.DateOffset(months=-6)
db_triangle["Año Ocurrencia"] = db_triangle["Año Ocurrencia"] + pd.DateOffset(months=-6)



triang_stos = cl.Triangle(
    db_triangle,
    origin="Año Ocurrencia",
    development="Año Valuacion",
    columns=["Pagados", "Pendientes", "Incurridos", "Cantidad"],
    index=["Ramo"],
    cumulative=True,
)
triang_facpce = cl.Triangle(
    db_triangle,
    origin="Año Ocurrencia",
    development="Año Valuacion",
    columns="Factor FACPSE",
    index=["Ramo"],
    cumulative=True,
)
triang_stos_act = triang_stos.cum_to_incr() * triang_facpce


df_triang = triang_stos_act.to_frame(keepdims=True)
print(df_triang.head(10))
# df_triang = triang_stos_act.incr_to_cum().link_ratio.to_frame(keepdims=True)
df_triang["development"] = df_triang["development"] / 12

dir_output = f'.{args.tri_input.split(sep = ".")[1]}.csv'
df_triang.to_csv(dir_output, encoding="latin-1")
print(f'Archivo creado en {dir_output}')
