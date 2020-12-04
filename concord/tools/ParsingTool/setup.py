from setuptools import setup

setup(
    name='cli',
    version='0.2',
    py_modules=['cli'],
    packages=['vm_parser', 'st_parser', 'utils'],
    install_requires=[
        'Click',
        'TQDM',
    ],
    entry_points='''
        [console_scripts]
        cli=cli:entry_point
    ''',
)