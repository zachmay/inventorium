truncate table building cascade;

insert into building (id, name, description) values
    (1, 'The Institute, Building A', '101 E. West St., Toronto'),
    (2, 'The Institute, Building B', '103 E. West St., Toronto'),
    (3, 'Ministry of Truth, Annex 1', '4 BB Circle, London');

insert into room (id, building, name, description) values
    (1, 1, '202', 'Russell'),
    (2, 1, '203', 'Whitehead'),
    (3, 1, '204', 'Wittgenstein'),
    (4, 3, 'Room 101', 'Interview Room 1');
