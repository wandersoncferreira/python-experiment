# python-experiment-mode
Package to make Experiments in Python way lot easier.

How many times you desired to have a dummy dictionary or dataframe just available to verify if that obscure operation you developed works or not?

I need this column on index to perform this group by in pandas? 

Well, these issues are addressed by this package.

# Installation
For now, there is only one way to install this package. 

(add-to-list 'load-path "~/python-experiment-mode")
(require 'python-experiment-mode)


If you desire to see this package in MELPA let me know in Issues.


# Usage

There are 4 recommended bindings in this package:
(global-set-key (kbd "<f9>") 'python-experiment)
(global-set-key (kbd "<f10>") 'python-experiment-lived-too-long)
(global-set-key (kbd "<f11>") 'python-experiment-reload)
(global-set-key (kbd "<f12>") 'python-experiment-buffer-to-file)


*<kbd>F9</kbd> will open a frame like this one below:



The Python Experiment Buffer is not attached to any file and you can easily edit it.


*<kbd>F10</kbd> will shut everything down!



*<kbd>F11</kbd> If you edit the Python Experiment Buffer to include any additional datatype, you can press F11 to reload the Inferior Python Process with the new additions.



*<kbd>F12</kbd> After a whole day of work you might have added several custom additions to your specific situation. Press *F12* to save the Python Experiment Buffer to a custom file that will be loaded in the next time.
