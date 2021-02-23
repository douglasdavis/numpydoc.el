# numpydoc.el

An Emacs Lisp package to automatically insert NumPy style docstrings
for Python functions. The `numpydoc-generate` function parses the
function signature and detects argument names, argument type hints,
and return type hints. The default behavior is to prompt the user (in
the minibuffer) for a (short and long) description of the function,
and a description for each argument and the returned value. If the
prompt is turned off some (customizable) template text will be
inserted into the docstring.

## Customization

<dl>
  <dt>numpydoc-prompt-for-input</dt>
  <dd>
  If true you will be prompted to enter a short description and long
  description, a description for each function argument, and a
  description of the return if a return type hint is provided (default
  is `t`).
  </dd>
  <dt>numpydoc-template-short</dt>
  <dd>
  Template text that will be used as the short description if
  `numpydoc-prompt-for-input` is `nil` (default is SHORT-DESCRIPTION).
  </dd>
  <dt>numpydoc-template-long</dt>
  <dd>
  Template text that will be used as the long description if
  `numpydoc-prompt-for-input` is `nil` (default is LONG-DESCRIPTION).
  </dd>
  <dt>numpydoc-template-desc</dt>
  <dd>
  (default `"ADD"`): Template text that will be used for each function
  argument description if `numpydoc-prompt-for-input` is `nil`.
  </dd>
  <dt>numpydoc-quote-char</dt>
  <dd>
  (default `?\"`): Python quote character to use (prefers double
  quite, the default from the black formatting tool).
  </dd>
  <dt>numpydoc-insert-examples-block</dt>
  <dd>
  (default `t`): If true an `Examples` block will be added to the
  docstring.
  </dd>
</dl>

## Example

An example function:

```python
def histogram(
    x: np.ndarray,
    bins: int = 10,
    range: Optional[Tuple[float, float]] = None,
    weights: Optional[np.ndarray] = None,
    flow: bool = False,
) -> Tuple[np.ndarray, np.ndarray]:
    pass
```

After <kbd>M-x numpydoc-generate</kbd>:

```python
def histogram(
    x: np.ndarray,
    bins: int = 10,
    range: Optional[Tuple[float, float]] = None,
    weights: Optional[np.ndarray] = None,
    flow: bool = False,
) -> Tuple[np.ndarray, np.ndarray]:
    """SHORT-SUMMARY

    LONG-SUMMARY

    Parameters
    ----------
    x : np.ndarray
       ADD
    bins : int
       ADD
    range : Optional[Tuple[float, float]]
       ADD
    weights : Optional[np.ndarray]
       ADD
    flow : bool
       ADD

    Returns
    -------
    Tuple[np.ndarray, np.ndarray]
        ADD

    Examples
    --------
    ADD

    """
    pass
```
