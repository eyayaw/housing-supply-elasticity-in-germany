import os
import sys
import re

# This script extracts notes from figure captions and places them in a minipage below the figure.
# The notes should be enclosed in a pair of triple angle brackets.
# #| fig-cap: A nice plot.<<<*Notes:* This is a note.>>>
# becomes: 
# ...
#   \caption{A nice plot.}
#   \begin{minipage}{0.975\textwidth}
#     \small
#     \emph{Notes:} This is a note.
#   \end{minipage}
# ...

# regex pattern to match the figure environment and extract notes from the caption. See https://regex101.com/r/RROSt7/3

regex = r"(?s)(?P<head>\\begin\{figure\}.*?(?:\\caption\{.*?))(?P<notes_delim>(?:\\textless){3}\{\}(?P<notes>\\(?:emph|textit)\{Notes:\}.*?)(?:\\textgreater){3}\{\})(?P<trailing>\})(?P<tail>.*?\\end\{figure\})"

subst = "\\g<head>\\g<trailing>\\n\\\\begin{minipage}{0.975\\\\textwidth}\\n\\\\small\\n\\g<notes>\\n\\\\end{minipage}\\n\\g<tail>"


def tidy_figure_notes(file_path, overwrite=True):
    """
    Extract notes from figure captions and place them in a minipage below the figure.
    """
    try:
        with open(file_path, "r") as file:
            file_text = file.read()
    except FileNotFoundError:
        print(f"File {file_path} not found.")
        return False

    result = re.sub(regex, subst, file_text, 0)

    if not overwrite:
        file_path = file_path.replace(".tex", "_tidy.tex")
    if not result:
        print("No substitutions were made.")
        return False

    print("Substitution has been made.")
    with open(file_path, "w") as file:
        file.write(result)
    return True


def main():
    file_path = sys.argv[1] # tex path
    out_path = sys.argv[2]  # pdf path
    if not out_path:
        out_path = file_path.replace(".tex", "_tidy.pdf")
    tidy_figure_notes(file_path, overwrite=True)
    print("Rendering PDF ...")
    os.system(f'Rscript -e \'tinytex::xelatex("{file_path}", pdf_file="{out_path}")\'')


if __name__ == "__main__":
    main()