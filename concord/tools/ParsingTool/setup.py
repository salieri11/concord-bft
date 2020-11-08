from setuptools import setup

setup(
    name='cli',
    version='0.1',
    py_modules=['cli'],
    packages=['vm_parser', 'utils'],
    install_requires=[
        'Click',
        'TQDM',
    ],
    entry_points='''
        [console_scripts]
        cli=cli:entry_point
    ''',
)