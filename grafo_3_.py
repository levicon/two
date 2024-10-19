import matplotlib.pyplot as plt
import networkx as nx
import numpy as np

# Datos de ejemplo para los nodos y matriz de pesos
etiquetas = ['29s', '39s', '49s', '59s', '69s', '79s', '80+']
pesos_usuario = np.array([
    [1.000500157, 0.001322106, 0.001626555, 0.001514414, 0.001003487, 0.000410827, 0.000137880],
    [0.000418173, 1.001104961, 0.001363461, 0.001270093, 0.000840877, 0.000344894, 0.000115847],
    [0.000424180, 0.001119386, 1.001382498, 0.001287767, 0.000851824, 0.000349339, 0.000117153],
    [0.000424753, 0.001122759, 0.001386288, 1.001291615, 0.000855017, 0.000350805, 0.000117705],
    [0.000397544, 0.001052962, 0.001303416, 0.001214981, 1.000804169, 0.000330531, 0.000111054],
    [0.000411723, 0.001101299, 0.001367571, 0.001275975, 0.000846259, 1.000349043, 0.000117754],
    [0.000398793, 0.001061115, 0.001315032, 0.001226974, 0.000813749, 0.000335527, 1.000112973]
])

# Crear el grafo con NetworkX
G_usuario = nx.Graph()

# Añadir los nodos y aristas basados en los pesos
for i in range(len(etiquetas)):
    for j in range(i+1, len(etiquetas)):
        G_usuario.add_edge(etiquetas[i], etiquetas[j], weight=pesos_usuario[i, j])

# Posición de los nodos utilizando un layout
pos_compacto = nx.spring_layout(G_usuario, seed=42, k=0.1)

# Identificar el nodo con el mayor peso (grupo "49s")
grupo_mayor_aporte = '49s'

# Ajustar el tamaño y color de los nodos, destacando "49s" en gris pastel
node_sizes_personalizado = [2000 if etiqueta == grupo_mayor_aporte else 700 for etiqueta in etiquetas]
node_colors_personalizado_gris = ['lightgray' if etiqueta == grupo_mayor_aporte else 'lightblue' for etiqueta in etiquetas]

# Crear la figura con el ajuste de altura
fig, ax = plt.subplots(figsize=(1063/300, (1063/300 * (10 / 22.44)) + 2))

# Dibujar el grafo
nx.draw(G_usuario, pos=pos_compacto, with_labels=True, node_size=node_sizes_personalizado, node_color=node_colors_personalizado_gris, font_size=12, font_weight="bold", edge_color="gray", ax=ax)

# Añadir un recuadro negro (borde)
plt.gca().spines['top'].set_color('black')
plt.gca().spines['right'].set_color('black')
plt.gca().spines['bottom'].set_color('black')
plt.gca().spines['left'].set_color('black')

plt.gca().spines['top'].set_linewidth(2)
plt.gca().spines['right'].set_linewidth(2)
plt.gca().spines['bottom'].set_linewidth(2)
plt.gca().spines['left'].set_linewidth(2)

# Ajustar la disposición para que todo el grafo sea visible
plt.tight_layout()

# Guardar la imagen con el recuadro negro
file_path_con_recuadro = "/mnt/data/grafo_con_recuadro_1063px_300dpi.png"
plt.savefig(file_path_con_recuadro, dpi=300, format="png", bbox_inches='tight')

# Mostrar el gráfico
plt.show()

# El archivo está guardado en file_path_con_recuadro
