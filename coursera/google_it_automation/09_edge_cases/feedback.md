At 2:36 in the video, they show the following code:

```python
import re

def rearrange_name(name):
    result = re.search(r"^([\w .]*), ([\w .]*)$", name)
    if result is None:  # these two lines
        return ""       # were added
    return "{} {}".format(result[2], result[1])
```

The description of what this does is to "return the original empty string" if the input is an empty string.

This is not what the code does, and is misleading.

Code that matches the description would be:

```python
import re

def rearrange_name(name):
    # due to string interning, `is` could be used
    if name == "":
        return name
    result = re.search(r"^([\w .]*), ([\w .]*)$", name)
    return "{} {}".format(result[2], result[1])
```

This would still be flakey. Later, they say "In our specific case, returning the original value makes sense [...]". What they probably meant to do, to match the behaviour they are describing is:

```python
import re

def rearrange_name(name):
    result = re.search(r"^([\w .]*), ([\w .]*)$", name)
    if result is None:
        return name  # return input unchanged
    return "{} {}".format(result[2], result[1])
```

Update: this was an intentional error, to be encountered and addressed later. I don't like the initial incorrect description of the behaviour.
