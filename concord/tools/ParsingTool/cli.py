import click
from vm_parser import daml_parser
from st_parser import st_parser


@click.group()
def entry_point() -> None:
    """
        \b
        CLI helps to filter and produce a readable view of the logs
    """
    pass


entry_point.add_command(daml_parser.parse)
entry_point.add_command(st_parser.parse)
