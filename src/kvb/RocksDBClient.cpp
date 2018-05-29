// Copyright 2018 VMware, all rights reserved

/**
 * @file RocksDBClient.cc
 *
 * @brief Contains helper functions for the RocksDBClient and
 * RocksDBClientIterator classes.
 *
 * Wrappers around RocksDB functions for standard database operations.
 * Functions are included for creating, using, and destroying iterators to
 * navigate through the RocksDB Database.
 */

#ifdef USE_ROCKSDB

#include <log4cplus/loggingmacros.h>

#include "rocksdb/options.h"
#include "RocksDBClient.h"
#include "HexTools.h"

namespace Blockchain
{
/**
 * @brief Converts a Slice object to a RocksDB Slice object.
 *
 * @param _s Slice object.
 * @return A RocksDB Slice object.
 */
rocksdb::Slice toRocksdbSlice(Slice _s)
{
   return rocksdb::Slice(_s.data(), _s.size());
}

/**
 * @brief Converts a RocksDB Slice object to a Slice object.
 *
 * @param _s A RocksDB Slice object.
 * @return A Slice object.
 */
Slice fromRocksdbSlice(rocksdb::Slice _s)
{
   return Slice(_s.data(), _s.size());
}

/**
 * @brief Opens a RocksDB database connection.
 *
 * Uses the RocksDBClient object variables m_dbPath and m_dbInstance to
 * establish a connection with RocksDB by creating a RocksDB object.
 *
 *  @return GeneralError in case of error in connection, else OK.
 */
Status RocksDBClient::init()
{
   rocksdb::Options options;
   options.create_if_missing = true;
   options.comparator = m_comparator;

   rocksdb::Status s = rocksdb::DB::Open(options, m_dbPath, &m_dbInstance);

   if (!s.ok()) {
      LOG4CPLUS_ERROR(logger, "Failed to open rocksdb database at " <<
                      m_dbPath << " due to " << s.ToString());
      return Status::GeneralError("Database open error");
   }

   g_rocksdb_called_read = 0;

   // TODO(Shelly): Update for measurements. Remove when done as well as other
   // g_rocksdb_* variables.
   g_rocksdb_print_measurements = false;

   return Status::OK();
}

/**
 * @brief Closes a RocksDB connection.
 *
 * Deletes the RocksDB object to close the database connection.
 *
 * @return Status OK.
 */
Status RocksDBClient::close()
{
   delete m_dbInstance;
   return Status::OK();
}

/**
 * @brief Services a read request from the RocksDB database.
 *
 * Fires a get request to the RocksDB client and stores the data of the
 * response in a new Slice object.
 *
 * Note: the reference to the data is not stored, the data itself is.
 *
 * @param _key Slice object of the key that needs to be looked up.
 * @param _outValue Slice object in which the data of the Get response is
 *                  stored, if any.
 * @return Status NotFound if key is not present, Status GeneralError if error
 *         in Get, else Status OK.
 */
Status RocksDBClient::get(Slice _key, OUT Slice & _outValue) const
{
   ++g_rocksdb_called_read;
   if (g_rocksdb_print_measurements) {
      LOG4CPLUS_DEBUG(logger, "Reading count = " << g_rocksdb_called_read <<
                      ", key " << sliceToString(_key));
   }

   std::string value;
   rocksdb::Status s = m_dbInstance->Get(rocksdb::ReadOptions(),
                                         toRocksdbSlice(_key),
                                         &value);

   if (s.IsNotFound()) {
      return Status::NotFound("Not found");
   }

   if (!s.ok()) {
      LOG4CPLUS_DEBUG(logger, "Failed to get key " << sliceToString(_key) <<
                      " due to " << s.ToString());
      return Status::GeneralError("Failed to read key");
   }

   size_t valueSize = value.size();
   // Must copy the string data
   char *stringCopy = new char[valueSize];
   memcpy(stringCopy, value.data(), valueSize);
   _outValue = Slice(stringCopy, valueSize);

   return Status::OK();
}

/**
 * @brief Returns a RocksDBClientIterator object.
 *
 * @return RocksDBClientIterator object.
 */
IDBClient::IDBClientIterator * RocksDBClient::getIterator() const
{
   return new RocksDBClientIterator(this);
}

/**
 * @brief Frees the RocksDBClientIterator.
 *
 * @param _iter Pointer to object of class RocksDBClientIterator (the iterator)
 *              that needs to be freed.
 * @return Status InvalidArgument if iterator is null pointer, else, Status OK.
 */
Status RocksDBClient::freeIterator(IDBClientIterator* _iter) const
{
   if (_iter == NULL) {
      return Status::InvalidArgument("Invalid iterator");
   }

   RocksDBClientIterator *iter = (RocksDBClientIterator*)_iter;
   delete iter->m_iter;
   delete (RocksDBClientIterator*)_iter;

   return Status::OK();
}

/**
 * @brief Returns an iterator.
 *
 * Returns a reference to a new object of RocksDbIterator.
 *
 * @return A pointer to RocksDbIterator object.
 */
rocksdb::Iterator* RocksDBClient::getNewRocksDbIterator() const
{
   return m_dbInstance->NewIterator(rocksdb::ReadOptions());
}

/**
 * @brief Currently used to check the number of read requests received.
 */
void RocksDBClient::monitor() const
{
   //TODO Can be used for additional sanity checks and debugging.

   if (g_rocksdb_print_measurements) {
      LOG4CPLUS_DEBUG(logger, "No. of times read: " << g_rocksdb_called_read);
   }
}

/**
 * @brief Constructor for the RocksDBClientIterator class.
 *
 * Calls the getNewRocksDbIterator function.
 */
RocksDBClientIterator::RocksDBClientIterator(const RocksDBClient *_parentClient)
   : logger(log4cplus::Logger::getInstance("com.vmware.athena.kvb")),
     m_parentClient(_parentClient)
{
   m_iter = m_parentClient->getNewRocksDbIterator();
}

/**
 * @brief Services a write request to the RocksDB database.
 *
 * Fires a put request to the RocksDB client.
 *
 * @param _key The key that needs to be stored.
 * @param _value The value that needs to be stored against the key.
 * @return Status GeneralError if error in Put, else Status OK.
 */
Status RocksDBClient::put(Slice _key, Slice _value)
{
   rocksdb::WriteOptions woptions = rocksdb::WriteOptions();

   rocksdb::Status s = m_dbInstance->Put(
      woptions, toRocksdbSlice(_key), toRocksdbSlice(_value));

   LOG4CPLUS_DEBUG(logger, "Rocksdb Put " << sliceToString(_key) <<
                   " : " << sliceToString(_value));

   if (!s.ok()) {
      LOG4CPLUS_ERROR(logger, "Failed to put key " << sliceToString(_key) <<
                      ", value " << sliceToString(_value));
      return Status::GeneralError("Failed to put key");
   }

   return Status::OK();
}

/**
 * @brief Services a delete request to the RocksDB database.
 *
 *  Fires a Delete request to the RocksDB database.
 *
 *  @param _key The key corresponding to the key value pair which needs to be
 *              deleted.
 *  @return Status GeneralError if error in delete, else Status OK.
 */
Status RocksDBClient::del(Slice _key)
{
   rocksdb::WriteOptions woptions = rocksdb::WriteOptions();
   rocksdb::Status s = m_dbInstance->Delete(woptions, toRocksdbSlice(_key));

   LOG4CPLUS_DEBUG(logger, "Rocksdb delete " << sliceToString(_key));

   if (!s.ok()) {
      LOG4CPLUS_ERROR(logger, "Failed to delete key " << sliceToString(_key));
      return Status::GeneralError("Failed to delete key");
   }

   return Status::OK();
}

/**
 * @brief Deallocates the memory of the data part of a Slice object.
 *
 * @param _value The Slice object whose "data" needs to be deallocated.
 * @return Status OK.
 */
Status RocksDBClient::freeValue(Slice& _value)
{
   delete[] _value.data();
   return Status::OK();
}

/**
 * @brief Returns the KeyValuePair object of the first key in the database.
 *
 * @return The KeyValuePair object of the first key.
 */
KeyValuePair RocksDBClientIterator::first()
{
   ++g_rocksdb_called_read;
   if (g_rocksdb_print_measurements) {
      LOG4CPLUS_DEBUG(logger, "Reading count = " << g_rocksdb_called_read);
   }

   // Position at the first key in the database
   m_iter->SeekToFirst();

   if (!m_iter->Valid()) {
      LOG4CPLUS_ERROR(logger, "Did not find a first key");
      m_status = Status::NotFound("Empty database");
      return KeyValuePair();
   }

   Slice key = fromRocksdbSlice(m_iter->key());
   Slice value = fromRocksdbSlice(m_iter->value());

   m_status = Status::OK();
   return KeyValuePair(key, value);
}

/**
 * @brief Returns the key value pair of the key which is greater than or equal
 * to _searchKey.
 *
 * Returns the first key value pair whose key is not considered to go before
 * _searchKey. Also, moves the iterator to this position.
 *
 * @param _searchKey Key to search for.
 * @return Key value pair of the key which is greater than or equal to
 *         _searchKey.
 */
KeyValuePair RocksDBClientIterator::seekAtLeast(Slice _searchKey)
{
   ++g_rocksdb_called_read;
   if (g_rocksdb_print_measurements) {
      LOG4CPLUS_DEBUG(logger, "Reading count = " << g_rocksdb_called_read <<
                      ", key " << sliceToString(_searchKey));
   }

   m_iter->Seek(toRocksdbSlice(_searchKey));
   if (!m_iter->Valid()) {
      LOG4CPLUS_ERROR(logger, "Did not find search key " <<
                      sliceToString(_searchKey));
      // TODO(SG): Status to exception?
      return KeyValuePair();
   }

   // Apparently, this is transient.
   Slice key = fromRocksdbSlice(m_iter->key());
   Slice value = fromRocksdbSlice(m_iter->value());

   LOG4CPLUS_DEBUG(logger, "Key " << sliceToString(key) << " value " <<
                   sliceToString(value));
   m_status = Status::OK();
   return KeyValuePair(key,value);
}

/**
 * @brief Decrements the iterator.
 *
 * Decrements the iterator and returns the previous key value pair.
 *
 * @return The previous key value pair.
 */
KeyValuePair RocksDBClientIterator::previous()
{
   m_iter->Prev();

   if (!m_iter->Valid()) {
      LOG4CPLUS_ERROR(logger, "Iterator out of bounds");
      return KeyValuePair();
   }

   Slice key = fromRocksdbSlice(m_iter->key());
   Slice value = fromRocksdbSlice(m_iter->value());

   LOG4CPLUS_DEBUG(logger, "Key " << sliceToString(key) << " value " <<
                   sliceToString(value));
   m_status = Status::OK();

   return KeyValuePair(key,value);
}

/**
 * @brief Increments the iterator.
 *
 * Increments the iterator and returns the next key value pair.
 *
 * @return The next key value pair.
 */
KeyValuePair RocksDBClientIterator::next()
{
   ++g_rocksdb_called_read;
   if (g_rocksdb_print_measurements) {
      LOG4CPLUS_DEBUG(logger, "Reading count = " << g_rocksdb_called_read);
   }

   m_iter->Next();
   if (!m_iter->Valid()) {
      LOG4CPLUS_ERROR(logger, "No next key");
      m_status = Status::GeneralError("No next key");
      return KeyValuePair();
   }

   Slice key = fromRocksdbSlice(m_iter->key());
   Slice value = fromRocksdbSlice(m_iter->value());

   LOG4CPLUS_DEBUG(logger, "Key " << sliceToString(key) << " value " <<
                   sliceToString(value));
   m_status = Status::OK();
   return KeyValuePair(key, value);
}

/**
 * @brief Returns the key value pair at the current position of the iterator.
 *
 * @return Current key value pair.
 */
KeyValuePair RocksDBClientIterator::getCurrent()
{
   if (!m_iter->Valid()) {
      LOG4CPLUS_ERROR(logger, "Iterator is not pointing at an element");
      m_status = Status::GeneralError("Iterator is not pointing at an element");
      return KeyValuePair();
   }

   Slice key = fromRocksdbSlice(m_iter->key());
   Slice value = fromRocksdbSlice(m_iter->value());

   m_status = Status::OK();
   return KeyValuePair(key, value);
}

/**
 * @brief Tells whether iterator has crossed the bounds of the last key value
 * pair.
 *
 * @return True if iterator is beyond the bounds, else False.
 */
bool RocksDBClientIterator::isEnd()
{
   return !m_iter->Valid();
}

/**
 * @brief Returns the Status.
 *
 * @return The latest Status logged.
 */
Status RocksDBClientIterator::getStatus()
{
   return m_status;
}
}
#endif
