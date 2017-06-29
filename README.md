# WUtils

WUtils is a Wolfram Language package containing a variety of functions that I written over the years.

Assuming you have the `WUtils` directory on your `$Path`, you can do:

```
Get["WUtils`"]
```

## Indent2

The function I use the most is `Indent2`, which can be used to indent a Wolfram Language expression for improved readability.

It is most often used with `//` as follows:

```
... // Indent2
```

## Unit Testing Tools

This package contains an implementation of a notebook-based unit testing toolset that can significantly reduce the amount of time it takes to write a new function and unit test it.  When paired with a natural language UI (see my `Lui` project), you can create a new function like this:

    new util func AddNumbers[a, b] "Adds two numbers."

... where `util` in this case is a pre-registered linguistic that identifies what source file you want to contain the new function.

This will create a template for your new function in the given file, open that file in workbench (if you have the workbench plugin installed located in the `WorkbenchPlugin` directory), and create a notebook for interactively running your function.

If you have previously set ```Global`$EnableAutoExport = True``` , then this will also export the function because it starts with a capital letter.

The created function notebook then allows you to click `Add Unit Tests` to create/update a .mt file containing tests.