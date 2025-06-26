import pandas as pd
import seaborn as sns
import matplotlib.pyplot as plt
from matplotlib.backends.backend_tkagg import FigureCanvasTkAgg
import tkinter as tk
from tkinter import ttk, messagebox, filedialog

# --- Definicje stylów dla nowoczesnego wyglądu ---
BG_COLOR = "#F0F2F5"
FRAME_BG_COLOR = "#FFFFFF"
ACCENT_COLOR = "#0078D7"
TEXT_COLOR = "#212121"
FONT_BODY = ("Segoe UI", 10)
FONT_TITLE = ("Segoe UI", 12, "bold")
FONT_MONO = ("Consolas", 9)


class PlotFrame(ttk.Frame):
    def __init__(self, master):
        super().__init__(master, style="Card.TFrame", padding=5)
        self.fig, self.ax = plt.subplots(facecolor=FRAME_BG_COLOR)
        self.ax.set_facecolor(FRAME_BG_COLOR)
        self.canvas = FigureCanvasTkAgg(self.fig, master=self)
        self.canvas_widget = self.canvas.get_tk_widget()
        self.canvas_widget.pack(fill=tk.BOTH, expand=True)
        self.show_placeholder("Wybierz miejsce na wykres")

    def draw_plot(self, plot_function, **kwargs):
        self.ax.clear()
        plot_function(ax=self.ax, **kwargs)
        self.fig.tight_layout(pad=2.0)
        self.canvas.draw()

    def show_placeholder(self, text):
        self.ax.clear()
        self.ax.text(0.5, 0.5, text, ha='center', va='center', fontsize=12, color='gray')
        self.fig.tight_layout(pad=2.0)
        self.canvas.draw()


class DataAnalyzerApp:
    def __init__(self, root):
        self.root = root
        self.root.title("Analizator Danych AI")
        self.root.geometry("1400x900")
        self.root.configure(bg=BG_COLOR)
        self.root.state('zoomed')

        self.df = None
        self.radio_str_var = tk.StringVar()
        self.var_4_2 = tk.IntVar(value=1)

        self.stat_col_var = tk.StringVar()
        self.stat_func_var = tk.StringVar()
        self.result_var = tk.StringVar(value="Wynik pojawi się tutaj")

        self.setup_styles()
        self.create_widgets()

    def setup_styles(self):
        style = ttk.Style(self.root)
        style.theme_use('clam')
        style.configure(".", background=BG_COLOR, foreground=TEXT_COLOR, font=FONT_BODY)
        style.configure("TFrame", background=BG_COLOR)
        style.configure("Card.TFrame", background=FRAME_BG_COLOR, relief='solid', borderwidth=1)
        style.configure("TLabel", background=BG_COLOR)
        style.configure("Card.TLabel", background=FRAME_BG_COLOR)
        style.configure("Result.TLabel", font=FONT_TITLE, background=FRAME_BG_COLOR)
        style.configure("TButton", background=ACCENT_COLOR, foreground="white", font=FONT_BODY, borderwidth=0)
        style.map("TButton", background=[('active', '#005a9e')])
        style.configure("TEntry", fieldbackground=FRAME_BG_COLOR)
        style.configure("TCombobox", fieldbackground=FRAME_BG_COLOR)
        style.configure("TRadiobutton", background=FRAME_BG_COLOR)
        style.configure("TLabelFrame", background=FRAME_BG_COLOR)
        style.configure("TLabelFrame.Label", background=FRAME_BG_COLOR, foreground=TEXT_COLOR, font=FONT_TITLE)

    def create_widgets(self):
        main_frame = ttk.Frame(self.root, padding=10)
        main_frame.pack(fill=tk.BOTH, expand=True)
        main_frame.grid_columnconfigure(1, weight=1)
        main_frame.grid_rowconfigure(0, weight=1)

        control_panel = ttk.Frame(main_frame, width=350)
        control_panel.grid(row=0, column=0, sticky="ns", padx=(0, 10))
        control_panel.grid_propagate(False)

        display_panel = ttk.Frame(main_frame)
        display_panel.grid(row=0, column=1, sticky="nsew")
        display_panel.grid_rowconfigure(0, weight=1)
        display_panel.grid_rowconfigure(1, weight=2)
        display_panel.grid_columnconfigure(0, weight=1)

        # 1. Ładowanie Danych
        load_frame = ttk.LabelFrame(control_panel, text="1. Plik Danych", padding=10)
        load_frame.pack(fill=tk.X, pady=5)
        self.entry_1_1 = ttk.Entry(load_frame)
        self.entry_1_1.insert(0, "data/ai_job_dataset.csv")
        self.entry_1_1.pack(side=tk.LEFT, expand=True, fill=tk.X, padx=(0, 5))
        ttk.Button(load_frame, text="Przeglądaj...", command=self.browse_file).pack(side=tk.LEFT)

        ttk.Button(control_panel, text="Załaduj Plik", command=self.get_data).pack(fill=tk.X, pady=(5, 10))
        # === ZMIANA TUTAJ: Zmieniono tekst przycisku
        ttk.Button(control_panel, text="Pokaż Podgląd Danych", command=self.show_data_preview).pack(fill=tk.X,
                                                                                                    pady=(0, 10))

        # 2. Wybór Atrybutów
        cols_frame = ttk.LabelFrame(control_panel, text="2. Atrybuty Wykresu", padding=10)
        cols_frame.pack(fill=tk.BOTH, expand=True, pady=5)
        cols_frame.grid_columnconfigure((0, 1), weight=1)
        cols_frame.grid_rowconfigure(1, weight=1)
        ttk.Label(cols_frame, text="Atrybut X:", style="Card.TLabel").grid(row=0, column=0, sticky='w')
        self.first_attribute_list = tk.Listbox(cols_frame, exportselection=False, font=FONT_BODY, relief='solid',
                                               borderwidth=1, selectbackground=ACCENT_COLOR)
        self.first_attribute_list.grid(row=1, column=0, sticky='nsew', padx=(0, 5))
        ttk.Label(cols_frame, text="Atrybut Y:", style="Card.TLabel").grid(row=0, column=1, sticky='w')
        self.second_attribute_list = tk.Listbox(cols_frame, exportselection=False, font=FONT_BODY, relief='solid',
                                                borderwidth=1, selectbackground=ACCENT_COLOR)
        self.second_attribute_list.grid(row=1, column=1, sticky='nsew', padx=(5, 0))

        # 3. Typ Wykresu
        plot_type_frame = ttk.LabelFrame(control_panel, text="3. Rodzaj Wykresu", padding=10)
        plot_type_frame.pack(fill=tk.X, pady=5)
        radio_buttons_names_lt = ['line_plot', 'box_plot', 'bar_plot', 'scatter_plot', 'hist_plot']
        for i, name in enumerate(radio_buttons_names_lt):
            text = name.replace('_', ' ').title()
            ttk.Radiobutton(plot_type_frame, text=text, variable=self.radio_str_var, value=name).grid(row=i // 3,
                                                                                                      column=i % 3,
                                                                                                      sticky='w',
                                                                                                      padx=5)

        # 4. Akcje Wykresu
        action_frame = ttk.LabelFrame(control_panel, text="4. Akcje Wykresu", padding=10)
        action_frame.pack(fill=tk.X, pady=5)
        action_frame.grid_columnconfigure((0, 1), weight=1)
        ttk.Radiobutton(action_frame, text="Miejsce #1", variable=self.var_4_2, value=1).grid(row=0, column=0)
        ttk.Radiobutton(action_frame, text="Miejsce #2", variable=self.var_4_2, value=2).grid(row=0, column=1)
        ttk.Button(action_frame, text="Stwórz Wykres", command=self.update_plot).grid(row=1, column=0, pady=5,
                                                                                      sticky='ew')
        ttk.Button(action_frame, text="Wyczyść Wykres", command=self.clear_plot).grid(row=1, column=1, pady=5,
                                                                                      sticky='ew')

        # 5. Statystyki
        stats_frame = ttk.LabelFrame(control_panel, text="5. Statystyki Kolumny", padding=10)
        stats_frame.pack(fill=tk.X, pady=5)
        stats_frame.grid_columnconfigure(1, weight=1)
        ttk.Label(stats_frame, text="Kolumna:", style="Card.TLabel").grid(row=0, column=0, sticky='w', pady=2)
        self.stat_col_box = ttk.Combobox(stats_frame, textvariable=self.stat_col_var, state="readonly")
        self.stat_col_box.grid(row=0, column=1, sticky='ew', padx=5, pady=2)
        ttk.Label(stats_frame, text="Funkcja:", style="Card.TLabel").grid(row=1, column=0, sticky='w', pady=2)
        stat_func_box = ttk.Combobox(stats_frame, textvariable=self.stat_func_var, state="readonly")
        stat_func_box['values'] = ['Średnia', 'Maksimum', 'Minimum', 'Mediana', 'Q1', 'Q3']
        stat_func_box.current(0)
        stat_func_box.grid(row=1, column=1, sticky='ew', padx=5, pady=2)
        ttk.Button(stats_frame, text="Oblicz", command=self.calculate_stat).grid(row=2, column=0, columnspan=2,
                                                                                 sticky='ew', pady=(10, 5))
        ttk.Label(stats_frame, textvariable=self.result_var, style="Result.TLabel", anchor='center').grid(row=3,
                                                                                                          column=0,
                                                                                                          columnspan=2,
                                                                                                          pady=5)

        # Panel wyświetlania
        # === ZMIANA TUTAJ: Zmieniono tytuł ramki
        desc_frame = ttk.LabelFrame(display_panel, text="Podgląd Danych", padding=10)
        desc_frame.grid(row=0, column=0, sticky="nsew", pady=(0, 10))
        desc_frame.grid_rowconfigure(0, weight=1)
        desc_frame.grid_columnconfigure(0, weight=1)
        self.data_preview_widget = tk.Text(desc_frame, wrap="none", font=FONT_MONO, relief='solid', borderwidth=0,
                                           bg=FRAME_BG_COLOR)
        self.data_preview_widget.grid(row=0, column=0, sticky="nsew")
        ysb = ttk.Scrollbar(desc_frame, orient='vertical', command=self.data_preview_widget.yview)
        ysb.grid(row=0, column=1, sticky='ns')
        xsb = ttk.Scrollbar(desc_frame, orient='horizontal', command=self.data_preview_widget.xview)
        xsb.grid(row=1, column=0, sticky='ew')
        self.data_preview_widget.config(yscrollcommand=ysb.set, xscrollcommand=xsb.set, state='disabled')

        plots_container = ttk.Frame(display_panel)
        plots_container.grid(row=1, column=0, sticky="nsew")
        plots_container.grid_columnconfigure((0, 1), weight=1)
        plots_container.grid_rowconfigure(0, weight=1)
        self.plot_frame_5_1 = PlotFrame(plots_container)
        self.plot_frame_5_1.grid(row=0, column=0, sticky="nsew", padx=(0, 5))
        self.plot_frame_5_2 = PlotFrame(plots_container)
        self.plot_frame_5_2.grid(row=0, column=1, sticky="nsew", padx=(5, 0))

    def browse_file(self):
        filename = filedialog.askopenfilename(
            title="Wybierz plik CSV",
            filetypes=(("Pliki CSV", "*.csv"), ("Wszystkie pliki", "*.*"))
        )
        if filename:
            self.entry_1_1.delete(0, tk.END)
            self.entry_1_1.insert(0, filename)

    def get_data(self):
        entry_text = self.entry_1_1.get()
        if not entry_text:
            messagebox.showwarning("Brak ścieżki",
                                   "Proszę podać ścieżkę do pliku lub wybrać plik za pomocą przycisku 'Przeglądaj...'.")
            return

        try:
            self.df = pd.read_csv(entry_text)
            messagebox.showinfo(title="Proces pobierania danych", message="Dane zostały pomyślnie załadowane.")

            self.first_attribute_list.delete(0, tk.END)
            self.second_attribute_list.delete(0, tk.END)
            for col in self.df.columns:
                self.first_attribute_list.insert(tk.END, col)
                self.second_attribute_list.insert(tk.END, col)

            numeric_cols = self.df.select_dtypes(include='number').columns.tolist()
            self.stat_col_box['values'] = numeric_cols
            if numeric_cols:
                self.stat_col_box.current(0)

            # Automatycznie pokaż podgląd po załadowaniu
            self.show_data_preview()

        except FileNotFoundError:
            messagebox.showerror(title="FileNotFoundError", message="Nie znaleziono pliku pod podaną ścieżką.")
        except Exception as e:
            messagebox.showerror("Błąd", f"Wystąpił nieoczekiwany błąd: {e}")

    # === ZMIANA TUTAJ: Zmieniono całą funkcję, aby pokazywała podgląd, a nie statystyki
    def show_data_preview(self):
        if self.df is None:
            messagebox.showwarning("Ostrzeżenie", "Ramka danych nie jest jeszcze załadowana.")
            return
        try:
            # Ustawienia dla lepszego wyświetlania
            pd.set_option('display.width', 1000)
            pd.set_option('display.max_columns', None)

            # Pobierz 20 pierwszych wierszy jako tekst
            preview_text = self.df.head(20).to_string()

            self.data_preview_widget.config(state='normal')
            self.data_preview_widget.delete('1.0', tk.END)
            self.data_preview_widget.insert('1.0', preview_text)
            self.data_preview_widget.config(state='disabled')
        except Exception as e:
            messagebox.showerror(title="Błąd", message=f'Nie można wyświetlić podglądu danych: {e}')

    def calculate_stat(self):
        if self.df is None:
            messagebox.showwarning("Brak danych", "Najpierw załaduj plik CSV.")
            return

        col = self.stat_col_var.get()
        stat = self.stat_func_var.get()
        if not col or not stat:
            messagebox.showwarning("Brak wyboru", "Wybierz kolumnę i funkcję statystyczną.")
            return

        try:
            series = self.df[col].dropna()
            val = None
            if stat == 'Średnia':
                val = series.mean()
            elif stat == 'Maksimum':
                val = series.max()
            elif stat == 'Minimum':
                val = series.min()
            elif stat == 'Mediana':
                val = series.median()
            elif stat == 'Q1':
                val = series.quantile(0.25)
            elif stat == 'Q3':
                val = series.quantile(0.75)

            self.result_var.set(f"{stat}({col}) = {val:,.2f}".replace(',', ' '))
        except Exception as e:
            messagebox.showerror("Błąd obliczeń", f"Nie udało się obliczyć statystyki:\n{e}")

    def update_plot(self):
        if self.df is None:
            messagebox.showwarning("Brak danych", "Najpierw załaduj plik CSV.")
            return

        try:
            x_4_1 = self.first_attribute_list.get(self.first_attribute_list.curselection())
            y_4_1 = self.second_attribute_list.get(self.second_attribute_list.curselection())
            kind_4_1 = self.radio_str_var.get()
            place = self.var_4_2.get()
        except tk.TclError:
            messagebox.showwarning("Ostrzeżenie", "Proszę wybrać atrybuty dla X i Y.")
            return

        if not all([x_4_1, y_4_1, kind_4_1, place]):
            messagebox.showwarning("Ostrzeżenie", "Proszę wybrać atrybuty X, Y, rodzaj wykresu i miejsce.")
            return

        selected_placement = self.plot_frame_5_1 if place == 1 else self.plot_frame_5_2

        plot_functions = {
            "line_plot": sns.lineplot, "box_plot": sns.boxplot, "bar_plot": sns.barplot,
            "scatter_plot": sns.scatterplot, "hist_plot": sns.histplot
        }

        try:
            plot_func = plot_functions.get(kind_4_1)
            if plot_func:
                plot_args = {'data': self.df, 'x': x_4_1}
                if kind_4_1 != 'hist_plot':
                    plot_args['y'] = y_4_1
                else:
                    plot_args['kde'] = True

                selected_placement.draw_plot(plot_func, **plot_args)
                title = f"{kind_4_1.replace('_', ' ').title()}"
                selected_placement.ax.set_title(title)
        except Exception as e:
            messagebox.showerror("Błąd rysowania", f"Nie można stworzyć wykresu: {e}")

    def clear_plot(self):
        place = self.var_4_2.get()
        if place == 1:
            self.plot_frame_5_1.show_placeholder("Obszar #1 wyczyszczony")
        elif place == 2:
            self.plot_frame_5_2.show_placeholder("Obszar #2 wyczyszczony")
        else:
            messagebox.showwarning("Ostrzeżenie", "Proszę wybrać obszar do wyczyszczenia.")


if __name__ == "__main__":
    win_1 = tk.Tk()
    app = DataAnalyzerApp(win_1)
    win_1.mainloop()