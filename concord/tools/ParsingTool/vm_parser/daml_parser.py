from vm_parser.abstract_vm_parser import AbstractVmParser
from typing import TextIO
import click


class DamlParser(AbstractVmParser):

    def __init__(self, first_state, second_state):
        super().__init__("%Y-%m-%dT%H:%M:%S,%fZ", first_state, second_state,
                         '\d+-\d+-\d+T\d+:\d+:\d+,\d+Z', "correlationId=\w+[-\w]+")


@click.command('daml_parser', short_help='This command parse daml logs')
@click.argument("input_file", type=click.File('r'))
@click.argument('action')
@click.option('--thld', default=500, metavar='<int>',
              help='Pass threshold in milliseconds explicitly')
def parse(input_file: TextIO, action: str, thld: int) -> None:
    """
        \b
        This command parse daml logs
        \b
        failed-requests             This action prints requests that failed to process or submit
        exceeded-threshold          This action prints requests that passed the threshold
    """

    logs = input_file.readlines()
    if action == "failed-requests":
        parser = DamlParser("Processing blockId", "Submission succeeded")
        parser.failed_requests(logs)
    elif action == "exceeded-threshold":
        parser = DamlParser("Sending commit request", "Submission succeeded")
        parser.exceeded_threshold(logs, thld)
    else:
        raise click.BadParameter(f"'{action}', Try 'cli daml_parser --help' for help.")
