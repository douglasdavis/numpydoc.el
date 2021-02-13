# numpydoc.el

An Emacs Lisp package to automatically insert NumPy style docstrings
for Python functions.

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
