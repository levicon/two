import matplotlib.pyplot as plt
import networkx as nx
import numpy as np

# Cargar los nuevos datos proporcionados de diferencias
diferencias = np.array([
    [0.000000093, 0.000000249, 0.000000308, 0.000000287, 0.000150742, 0.000061714, 0.000020712],
    [0.000000078, 0.000000208, 0.000000258, 0.000000241, 0.000126315, 0.000051810, 0.000017403],
    [0.000000079, 0.000000210, 0.000000262, 0.000000244, 0.000127959, 0.000052477, 0.000017599],
    [0.000000080, 0.000000212, 0.000000262, 0.000000244, 0.000128439, 0.000052697, 0.000017682],
    [0.000000075, 0.000000199, 0.000000247, 0.000000230, 0.000120801, 0.000049652, 0.000016682],
    [0.000000079, 0.000000210, 0.000000260, 0.000000243, 0.000127124, 0.000052433, 0.000017689],
    [0.000000076, 0.000000202, 0.000000250, 0.000000233, 0.000122240, 0.000050402, 0.000016970]
])

# Etiquetas de los nodos
etiquetas_diferencias = ['29s', '39s', '49s', '59s', '69s', '79s', '80+']

# Crear el grafo utilizando los nuevos datos de diferencias
G_diferencias = nx.Graph()

# Añadir los nodos y aristas basados en los datos de diferencias
for i in range(len(etiquetas_diferencias)):
    G_diferencias.add_node(etiquetas_diferencias[i])
    for j in range(i+1, len(etiquetas_diferencias)):
        G_diferencias.add_edge(etiquetas_diferencias[i], etiquetas_diferencias[j], weight=diferencias[i, j])

# Crear una nueva disposición (layout) del grafo
pos_diferencias = nx.spring_layout(G_diferencias, seed=42)

# Identificar el nodo con mayor peso total
suma_total_columnas_diferencias = diferencias.sum(axis=0)
grupo_mayor_peso_diferencias = etiquetas_diferencias[np.argmax(suma_total_columnas_diferencias)]

# Ajustar el tamaño del nodo con mayor peso y reducir el tamaño de los demás
node_sizes_diferencias_pequeno = [1000 if etiqueta == grupo_mayor_peso_diferencias else 350 for etiqueta in etiquetas_diferencias]

# Ajustar los colores y tamaños para las relaciones importantes (umbral definido)
umbral_diferencias = 0.00012
edge_colors_diferencias = ['red' if diferencias[i, j] > umbral_diferencias else 'gray' 
                           for i in range(len(etiquetas_diferencias)) 
                           for j in range(i+1, len(etiquetas_diferencias))]
edge_widths_diferencias = [2 if diferencias[i, j] > umbral_diferencias else 0.5 
                           for i in range(len(etiquetas_diferencias)) 
                           for j in range(i+1, len(etiquetas_diferencias))]

# Crear la figura con las dimensiones solicitadas (1063 píxeles de ancho, 300 dpi)
fig, ax = plt.subplots(figsize=(1063/300, 1063/300 * (7/10)))

# Dibujar el grafo con los nodos más pequeños y nodos con mayor interrelación más cercanos
nx.draw(G_diferencias, pos=pos_diferencias, with_labels=True, node_size=node_sizes_diferencias_pequeno, node_color="lightblue", 
        font_size=8, font_weight="bold", edge_color=edge_colors_diferencias, width=edge_widths_diferencias, ax=ax)

# Guardar la imagen en 1063 píxeles y 300 dpi
file_path_1063px_small_circles = "/mnt/data/grafo_diferencias_1063px_300dpi_small_circles.png"
plt.savefig(file_path_1063px_small_circles, dpi=300, format="png", bbox_inches='tight')

# Mostrar el gráfico
plt.show()
