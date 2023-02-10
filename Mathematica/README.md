# GRHelper

The functions within the GRHelper package were originally written by Chuck Evans (and some of his grad students) @ UNC Chapel Hill.

David Brown @ NC State created the GRHelper package and tidied things up and put those functions into the package form you see here.

Bob Seaton @ NC State wanted a prettier output for the Christoffel Symbols, so he added that function and checked it into github.
## Usage

Inside the Mathematica Desktop app, locate and open the **usingChristoffelSymbols.nb** file (open it inside Mathematica!). It has examples you can play with. But before you can use that file, you'll need to [install the GRHelper package](README-InstallPackage.md).

To use any function in the GRHelper package, do like you were instructed above and **Install the package first**! Once that's done, then you can use any of the functions in the package.

## Function: ChristoffelSymbols

First, bring all of the GRHelper references into the current $Context with the "<< GRHelper`" command.
### Default settings

To use the ChristoffelSymbols function, you'll need to define a metric and a set of coordinates.  The image below shows what the results from default settings look like.

<img src="images/Example1.png" width=50%>

The next image shows how all of the options can be used. It is also possible to define a new variable to represent the "ChristoffelSymbol" function name -- Mathematica doesn't care and neither do I :) .

<img src="images/Example2.png" width=50%>

## ChristoffelSymbols Options

<table>
    <thead>
        <tr>
            <th>Option</th>
            <th>Default</th>
            <th>Usage</th>
            <th>Description</th>
        </tr>
    </thead>
    <tbody>
        <tr>
            <td>FontSize</td>
            <td>16</td>
            <td>FontSize -> 22</td>
            <td>Set the font size (measured in points) of the output</td>
        </tr>
        <tr>
            <td>FontFamily</td>
            <td>"American Typewriter</td>
            <td>FontFamily -> "Courier"</td>
            <td>Set the font family of the output. You can choose from whatever font family your machine supports.</td>
        </tr>
        <tr>
            <td>Header</td>
            <td>True</td>
            <td>Header -> False}</td>
            <td>If set "False", the header won't be displayed</td>
        </tr>
        <tr>
            <td>RemoveZeros</td>
            <td>True</td>
            <td>RemoveZeros -> False</td>
            <td>If set to "True", any symbol that evaluates to zero will be removed. If false, all permutations of Christoffel Symbols will be shown</td>
        </tr>
    </tbody>
</table>
