import tkinter as tk
from tkinter import ttk


count = 0
countd = 0
prompt = 'Enter u for up, or d for down or q to clear.'
text = '_'


def clicked(): # without event because I use `command=` instead of `bind`
    global count

    count = count + 1

    label1.configure(text=f"{count} fish up")

def clickedmin():
    global countd

    countd = countd + 1

    label12.configure(text=f"{countd} fish fell")

def key(event):
    global count
    global countd

    if event.char=='u':
        count = count + 1
    if event.char=='d':
        countd = countd + 1
    if event.char=='q':
        count = 0
        countd = 0

    label1.configure(text=f"{count} fish up")
    label12.configure(text=f"{countd} fish fell")


windows = tk.Tk()
windows.title("Fish Enumerator")

label0 = tk.Label(windows, text = prompt, width=len(prompt), bg='yellow')
label0.grid(column=0, row=0)

label = tk.Label(windows, text="Number Passed Up")
label.grid(column=0, row=1)
#output
label1 = tk.Label(windows)
label1.grid(column=0, row=2)
#number fish going down
labeldwn = tk.Label(windows, text="Number Fell Back")
labeldwn.grid(column=1, row=1)
#output
label12 = tk.Label(windows)
label12.grid(column=1, row=2)


#custom_button = tk.Button(windows, text="Up", command=clicked,
#	width=50,
#	height=50)
#custom_button.grid(column=1, row=0)

#downbutton = tk.Button(windows, text="Down", command=clickedmin,
#	width=50,
#	height=50)
#downbutton.grid(column=2,row=0)

windows.bind_all('<Key>', key)

windows.mainloop()
