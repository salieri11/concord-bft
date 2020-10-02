class TransactionCreationError(Exception):
    def __init__(self, message, details):
        self.message = message
        self.details = details


class TransactionReadError(Exception):
    def __init__(self, message, details):
        self.message = message
        self.details = details


