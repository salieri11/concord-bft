"get blockchain id"
consortium_id='01d271f6-a235-4d34-82d5-2794365bf879';
select row_key, column_name from entity where column_name = 'helen.blockchain' AND row_key in (select to_row from link where from_row = consortium_id);
               row_key                |   column_name
--------------------------------------+------------------
 51ee59cf-a28a-426a-b4be-19737eb5c4b3 | helen.blockchain

"get replica"
blockchain_id='51ee59cf-a28a-426a-b4be-19737eb5c4b3'
select row_key, column_name from entity where row_key in (select to_row from link where from_row = blockchain_id);
               row_key                |  column_name
--------------------------------------+---------------
 5af272fc-917f-47c0-adf6-3bdec8b41d4e | helen.replica
 3efe6f06-97a8-4424-a718-bcae289a6eb8 | helen.replica
 83aa527e-8556-4ae6-929f-04a444a0c555 | helen.replica
 957c5e8d-1d3f-45fb-b4a1-70cf917416ab | helen.replica

 "delete blockchain id"
 delete from entity where row_key in (select to_row from link where from_row = blockchain_id);

"delete link row"
delete from link where from_row = blockchain_id;

"delete consortium id"
delete from entity where column_name = 'helen.blockchain' AND row_key in (select to_row from link where from_row = consortium_id);

"delete link row"
delete from link where from_row = consortium_id;
delete from link where to_row = consortium_id;

delete from entity where row_key = consortium_id;

"consortium for given org_id"
select row_key, column_name from entity where column_name = 'helen.consortium' AND row_key in (select to_row from link where from_row = 'c56e116e-c36f-4f7d-b504-f9a33955b853');
               row_key                |   column_name
--------------------------------------+------------------
 c272ffe5-6202-4df5-ab1b-8e966ba760af | helen.consortium
 f45d4fa0-57fe-4e59-bdeb-1aa556e783c5 | helen.consortium
 c9682a53-e6ab-4daa-8cef-8c3c2d77975f | helen.consortium
 ee08a4b9-dca0-43e0-964e-abcc9e2f6b76 | helen.consortium
(4 rows)

"get org info"
select * from entity where column_name = 'helen.organization' \gx

"grab task for org"
select * from entity where column_name = 'helen.task' AND row_key in (select to_row from link where from_row = 'c56e116e-c36f-4f7d-b504-f9a33955b853');