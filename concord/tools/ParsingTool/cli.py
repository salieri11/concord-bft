import click
from vm_parser import daml_parser


@click.group()
def entry_point() -> None:
    """
        \b
        CLI helps to filter and produce a readable view of the logs
    """
    pass


entry_point.add_command(daml_parser.parse)
