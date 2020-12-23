#########################################################################
# Copyright 2020 VMware, Inc.  All rights reserved. -- VMware Confidential
#########################################################################
import sys
import json
if 'hermes_util' in sys.modules.keys():
    import hermes_util.hermes_logging as hermes_logging
    import hermes_util.helper as helper
else:
    import util.hermes_logging as hermes_logging
    import util.helper as helper

log = hermes_logging.getMainLogger()


def _get_from_rocksdb(blockchain_id, node, mthd,
                      skip_start=False, skip_stop=False):
    '''
    Function to fetch specified information
    Args:
        blockchain_id: Blockchain id
        node: Node to start/stop.
        mthd: Imformation to fetch
        skip_start: True to skip start of node
        skip_stop: True to skip node stop
    Returns:
        True when the desired action is completed.
        False when fails perform the desired action.
    '''
    log.info('Getting {0} for {1}'.format(mthd, node))
    if not skip_stop:
        assert helper.node_start_stop(blockchain_id,
                                      node,
                                      helper.STOP_NODE
                                      ), 'Failed to STOP Node {}'.format(node)

    status = invk_rocksdbdata_mthd(node, blockchain_id, mthd)

    if not skip_start:
        assert helper.node_start_stop(blockchain_id,
                                      node,
                                      helper.START_NODE
                                      ), 'Failed to Start Node {}'.format(node)

    if not status:
        log.error('Unable to get {0} from {1}'.format(mthd, node))
        return False
    log.debug("Fetch {0} from {1} IP: {2}".format(mthd, node, status))

    return status


def invk_rocksdbdata_mthd(host, blockchainid, action):
    '''
    Executes required fetch action on rocksdbdata
    Args:
        host: Concord node ip
        blockchain_id: Blockchain id
        action: docker command.
    Returns:
        Version in '0.0.0.0000' format.
        False when unable to get version.
    '''
    version = False
    status = False
    version = helper.get_product_version(blockchainid, host)
    if version:
        cmd = 'image=$(docker images --format "{{.Repository}}" |'\
                'grep "concord-core");'
        docker_cmd = 'docker run -it --entrypoint="" --mount type=bind,'\
            'source=/mnt/data/rocksdbdata,' \
            'target=/concord/rocksdbdata $image:{0} '\
            '/concord/sparse_merkle_db_editor ' \
            '/concord/rocksdbdata {1}'.format(version, action)

        status = helper.exec_cmd_on_node(host, cmd+docker_cmd,
                                         blockchain_id=blockchainid)

        if 'Failed to execute command' in status:
            log.error('Error on {0}: {1}'.format(host, status))
            status = False

    return status


def rocksdbdata_node_restore(host, blockchainid, source_db):
    '''
    Restore rocksdbdata from given source and clean metadata
    Args:
        host: Concord node ip
        blockchain_id: Blockchain id
        source_db: Path of source Db.
    Returns:
        Version in '0.0.0.0000' format.
        False when unable to get version.
    '''
    version = False
    status = False
    version = helper.get_product_version(blockchainid, host)
    if version:
        cmd = 'image=$(docker images --format "{{.Repository}}" |'\
                        ' grep "concord-core");'
        docker_cmd = 'docker run -it --entrypoint="" --mount type=bind,'\
            'source={},target=/concord/rocksdbdata ' \
            '$image:{} /concord/sparse_merkle_db_editor'\
            ' /concord/rocksdbdata removeMetadata; ' \
            'rm -rf /config/concord/config-generated/genSec_*'.format(
                source_db, version)

        status = helper.exec_cmd_on_node(host, cmd+docker_cmd,
                                         blockchain_id=blockchainid)

        if 'Failed to execute command' in status:
            log.error('Error on {0}: {1}'.format(host, status))
            status = False

    return status


def get_block_id(blockchain_id, node,
                 skip_start=False, skip_stop=False):
    '''
    Function to get last block id on a node.
    Args:
        blockchain_id: Blockchain id
        node: Node to get last block id.
        skip_start: skips starting the node.
        skip_stop: skips stopping the node.
    Returns:
        LastBlockID when available.
        False when it fails to get block id.
    '''
    status = False
    mthd = "getLastBlockID"
    status = _get_from_rocksdb(blockchain_id,
                               node, mthd,
                               skip_start=skip_start,
                               skip_stop=skip_stop)

    if not status:
        log.error('Unable to get LastBlockID from {}'.format(node))
        return False

    log.debug("{} IP with block Id {}".format(node, status))
    block_id = json.loads(status)
    return block_id['lastBlockID']


def get_genesis_block_id(blockchain_id, node,
                         skip_start=False, skip_stop=False):
    '''
    Function to get the first (genesis) block id on a node.
    Args:
        blockchain_id: Blockchain id
        node: Node to get genesis block id.
        skip_start: skips starting the node.
        skip_stop: skips stopping the node.
    Returns:
        genesisBlockID when available.
        False when it fails to get block id.
    '''
    status = False
    mthd = "getGenesisBlockID"
    status = _get_from_rocksdb(blockchain_id,
                               node, mthd,
                               skip_start=skip_start,
                               skip_stop=skip_stop)
    if not status:
        log.error('Unable to get genesisBlockID from {}'.format(node))
        return False

    block_id = json.loads(status)
    return block_id['genesisBlockID']


def get_raw_block(blockchain_id, node, block_id,
                  skip_start=False, skip_stop=False):
    '''
    Function to get raw block for a given block id on a node.
    Args:
        blockchain_id: Blockchain id
        node: Node to check backup.
        block_id: block id for which raw block detail is taken.
    Returns:
        True when the backup is available.
        False when there are no backup.
    '''
    raw_block = False
    mthd = "getRawBlock"

    log.info('Getting raw block for {}'.format(node))

    raw_block = _get_from_rocksdb(blockchain_id, node, mthd,
                                  skip_start=skip_start,
                                  skip_stop=skip_stop)

    log.debug("{} IP with raw block {}".format(node, raw_block))
    if 'NotFoundException' in raw_block:
        return False

    return raw_block


def get_raw_block_range(blockchain_id, node, start_block, end_block,
                        skip_start=False, skip_stop=False):
    '''
    Function to get raw block data for a range of block ids.
    Args:
        blockchain_id: Blockchain id
        node: Node to read from.
        start_block: ID of first block in the range
        end_block: ID of final block + 1 (not included in results)
    Returns:
        Dictionary mapping block ID to hex encoded raw data
    '''
    block_range = False
    mthd = "getRawBlockRange" + " " + start_block + " " + end_block

    log.info('Getting raw block range for {}'.format(node))

    block_range = _get_from_rocksdb(blockchain_id, node, mthd,
                                    skip_start=skip_start,
                                    skip_stop=skip_stop)

    if 'NotFoundException' in block_range:
        log.info("No results returned for \
            block range {} to {}".format(start_block, end_block))
        return False

    '''
    Output is in the following format, where <num> is block id like '100'
    and <data> is hex-encoded binary raw data like '123456abcdef':
    {
    "rawBlock<num>": "0x<data>",
    "rawBlock<num>": "0x<data>",
    ...
    }
    '''
    output = json.loads(block_range)
    key_start = 'rawBlock'
    key_len = len(key_start)
    result = {}
    for key, val in output.items():
        assert key.startswith(key_start) and val.startswith('0x'), \
            "Unexpected block data: {}:{}".format(key, val)
        result[key[key_len:]] = val[2:]

    return result
