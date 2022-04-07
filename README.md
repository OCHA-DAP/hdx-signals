# pa-global-monitoring

## Reproducing this analysis

Create a new virtual environment and install the requirements with:

```shell
pip install -r requirements.txt
```

Install code in `src` using the command:

```shell
pip install -e .
```

To run the pipeline that downloads and processes the data, execute:

```shell
python src/main.py
```

To see runtime options, execute:

```shell
python src/main.py -h
```

## Development

All code is formatted according to black and flake8 guidelines.
The repo is set-up to use pre-commit.
Before you start developing in this repository, you will need to run

```shell
pre-commit install
```

You can run all hooks against all your files using

```shell
pre-commit run --all-files
```
