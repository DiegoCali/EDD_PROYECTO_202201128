# User Manual
Author: Diego Cali
## Before start:
- You are going to need 2 extra directories. A 'files' directory for input files and a 'outputs' directory for output reports.
- Put your input files in the 'files' directory before running the program so the binary can read them later.
## Main menu:
```
----------Welcome to Pixel Print Studio----------
 Select an option:
 0. Admin
 1. Log in
 2. Sign in
 3. Exit
 -------------------------------------------------
```
Select with a number any of the options that te program presents to you.
### Admin:
```
------------------Admin log in-------------------
 Enter your password:
 (or type 'exit' to return to the main menu)
 |
```
This option grants you access to the admin special menu. (Place the admin password: 'admin')
#### Admin menu:
```
 ---------------------Admin-----------------------
 Select an option:
 0. Show B-Tree
 1. Users operations (insert, remove, modify)
 2. Massive charge of users
 3. Reports
 4. Exit
 -------------------------------------------------
```
Select any option that you need, here is a short description that each of the options do:

- Show B-Tree: Generates a graph representation of the clients signed in. Example:

![clients image](/docs/imgs/clients.svg)

- User operations: Here you can modify each client, delete it or insert a new client.
```
-----------------Users operations----------------
 Select an option:
 0. Insert user
 1. Remove user
 2. Modify user
 3. Exit
 -------------------------------------------------
```
- Massive charge of users: Here you can read the files, that you previously put on the 'files' directory. The files read by this options are 'clients.json':
```json
[
    {
        "dpi": "2897315340401",
        "nombre_cliente": "Diego Felipe",
        "password": "123456"
    },
    {
        "dpi": "9021315340402",
        "nombre_cliente": "Pablo Ricardo",
        "password": "password"
    }
]
```
- Reports: this option as it says generates reports written on the console:
```
-----------------Admin reports-------------------
 Select an option:
 0. Show client info
 1. List clients (Amplitude traversal)
 2. Exit
 -------------------------------------------------
```
### Log In:
If there are clients previously signed, you can select this option to access its own personal menu (of the client).
```
---------------------Log in----------------------
 Enter your DPI (Or type r to return):
2897315340401
 Enter your password:
diego213
Welcome to Pixel Print Studio, 2897315340401: Diego cali
 -------------------------------------------------
 -------------------Main Menu---------------------
 Select an option:
 0. Navigate and manage images
 1. Massive charge of images
 2. Visual Reports
 3. Reports
 4. Log out
 -------------------------------------------------
```
Select any option displayed at the console, a brief description of them:

- Navigate and manage images: here you can work with every image in the AVL tree.
```
-----------------Images operations----------------
 Select an option:
 0. Insert new image
 1. Delete image
 2. Navigate all images
 3. Navigate by album
 4. Exit
 -------------------------------------------------
```
*When you insert an image you can generate it in the moment you finished it. \
*When you navigate all images you can generate any if you select it.

- Massive charge of images: the files read by this options are, 'img.json', 'layers.json', 'albums.json'. The only file that is obligatory is 'layers.json'.
```json
[
    {
        "id_capa": 0,
        "pixeles": [
            {
                "fila": 50,
                "columna": 0,
                "color": "#000000"
            },
            {
                "fila": 51,
                "columna": 0,
                "color": "#000000"
            },
            {
                "fila": 52,
                "columna": 0,
                "color": "#000000"
            },
            {
                "fila": 53,
                "columna": 0,
                "color": "#000000"
            }
        ]
    }
]
```
```json
[
    {
        "id":4,
        "capas":[0,8,6,9,10,11,12]
    },
    {
        "id":7,
        "capas":[0,8,6,9,10,11,12]
    }
]
```
```json
[
    {
        "nombre_album": "Album 2",
        "imgs": [2]
    },
    {
        "nombre_album": "Album 3",
        "imgs": []
    },
    {
        "nombre_album": "Album 4",
        "imgs": [4, 6]
    }
]
```
- Visual reports: select this option to generate any kind of report.
```
-------------------------------------------------
 Select an option:
 0. Graph of image avl
 1. Graph layer bst
 2. Graph albums
 3. Graph image and layer tree
 4. Exit
```
![image avl](/docs/imgs/image_avl.svg)
![layer bst](/docs/imgs/layers_bst.svg)
![albums](/docs/imgs/albums_list.svg)
![image layer subtree](/docs/imgs/image_layer_tree.svg)

- Reports: these reports are displayed in the console.
```
 -----------------User reports-------------------
 Select an option:
 0. Top 5 images with more layers
 1. Leaf layers
 2. Layers tree depth
 3. List layers
 4. Exit
 -------------------------------------------------
```
### Sign In:
If you are not in the system you can create your user here.
```
--------------------Sign in----------------------
 Enter your username:
Diego cali
 Enter your password:
diego213
 Enter your DPI:
2897315340401
Welcome to Pixel Print Studio, Diego cali
Your DPI is: 2897315340401
 User added successfully!
 -------------------------------------------------
```
### Exit:
Select this option to terminate the program.