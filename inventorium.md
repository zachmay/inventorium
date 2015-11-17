## POST /api/auth

#### Request:

- Supported content types are:

    - `application/json`

#### Response:

- Status code 201
- Headers: []

- Supported content types are:

    - `application/json`

- No response body

## GET /api/buildings


- This endpoint is sensitive to the value of the **Authorization** HTTP header.

#### GET Parameters:

- sort
     - **Values**: *[-]created, [-]updated, [-]description, [-]name*
     - **Description**: Optional sort key, use `-` for descending sort
     - This parameter is a **list**. All GET parameters with the name sort[] will forward their values in a list to the handler.

- expand
     - **Values**: *rooms*
     - **Description**: Optional related records to retrieve.
     - This parameter is a **list**. All GET parameters with the name expand[] will forward their values in a list to the handler.


#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json`

- No response body

## POST /api/buildings


- This endpoint is sensitive to the value of the **Authorization** HTTP header.

#### Request:

- Supported content types are:

    - `application/json`

#### Response:

- Status code 201
- Headers: []

- Supported content types are:

    - `application/json`

- No response body

## DELETE /api/buildings/:buildingId

#### Captures:

- *buildingId*: The ID of the building.


- This endpoint is sensitive to the value of the **Authorization** HTTP header.

#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json`

- Response body as below.

```javascript
[]
```

## GET /api/buildings/:buildingId

#### Captures:

- *buildingId*: The ID of the building.


- This endpoint is sensitive to the value of the **Authorization** HTTP header.

#### GET Parameters:

- expand
     - **Values**: *rooms*
     - **Description**: Optional related records to retrieve.
     - This parameter is a **list**. All GET parameters with the name expand[] will forward their values in a list to the handler.


#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json`

- No response body

## PUT /api/buildings/:buildingId

#### Captures:

- *buildingId*: The ID of the building.


- This endpoint is sensitive to the value of the **Authorization** HTTP header.

#### Request:

- Supported content types are:

    - `application/json`

#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json`

- No response body

## GET /api/buildings/:buildingId/inventory

#### Captures:

- *buildingId*: The ID of the building.


- This endpoint is sensitive to the value of the **Authorization** HTTP header.

#### GET Parameters:

- sort
     - **Values**: *[-]created, [-]updated, [-]checkin, [-]name*
     - **Description**: Optional sort key, use `-` for descending sort.
     - This parameter is a **list**. All GET parameters with the name sort[] will forward their values in a list to the handler.

- expand
     - **Values**: *checkins*
     - **Description**: Optional related records to retrieve.
     - This parameter is a **list**. All GET parameters with the name expand[] will forward their values in a list to the handler.


#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json`

- No response body

## GET /api/buildings/:buildingId/rooms

#### Captures:

- *buildingId*: The ID of the building.


- This endpoint is sensitive to the value of the **Authorization** HTTP header.

#### GET Parameters:

- sort
     - **Values**: *[-]created, [-]updated, [-]name, [-]description*
     - **Description**: Optional sort key, use `-` for descending sort.
     - This parameter is a **list**. All GET parameters with the name sort[] will forward their values in a list to the handler.

- expand
     - **Values**: *inventory*
     - **Description**: Optional related records to retrieve.
     - This parameter is a **list**. All GET parameters with the name expand[] will forward their values in a list to the handler.


#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json`

- No response body

## POST /api/buildings/:buildingId/rooms

#### Captures:

- *buildingId*: The ID of the building.


- This endpoint is sensitive to the value of the **Authorization** HTTP header.

#### Request:

- Supported content types are:

    - `application/json`

#### Response:

- Status code 201
- Headers: []

- Supported content types are:

    - `application/json`

- No response body

## DELETE /api/buildings/:buildingId/rooms/:roomId

#### Captures:

- *buildingId*: The ID of the building.
- *roomId*: The ID of the room.


- This endpoint is sensitive to the value of the **Authorization** HTTP header.

#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json`

- Response body as below.

```javascript
[]
```

## GET /api/buildings/:buildingId/rooms/:roomId

#### Captures:

- *buildingId*: The ID of the building.
- *roomId*: The ID of the room.


- This endpoint is sensitive to the value of the **Authorization** HTTP header.

#### GET Parameters:

- expand
     - **Values**: *inventory*
     - **Description**: Optional related records to retrieve.
     - This parameter is a **list**. All GET parameters with the name expand[] will forward their values in a list to the handler.


#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json`

- No response body

## PUT /api/buildings/:buildingId/rooms/:roomId

#### Captures:

- *buildingId*: The ID of the building.
- *roomId*: The ID of the room.


- This endpoint is sensitive to the value of the **Authorization** HTTP header.

#### Request:

- Supported content types are:

    - `application/json`

#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json`

- No response body

## GET /api/buildings/:buildingId/rooms/:roomId/inventory

#### Captures:

- *buildingId*: The ID of the building.
- *roomId*: The ID of the room.


- This endpoint is sensitive to the value of the **Authorization** HTTP header.

#### GET Parameters:

- sort
     - **Values**: *[-]created, [-]updated, [-]checkin, [-]name*
     - **Description**: Optional sort key, use `-` for descending sort.
     - This parameter is a **list**. All GET parameters with the name sort[] will forward their values in a list to the handler.

- expand
     - **Values**: *checkins*
     - **Description**: Optional related records to retrieve.
     - This parameter is a **list**. All GET parameters with the name expand[] will forward their values in a list to the handler.


#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json`

- No response body

## POST /api/buildings/:buildingId/rooms/:roomId/inventory

#### Captures:

- *buildingId*: The ID of the building.
- *roomId*: The ID of the room.


- This endpoint is sensitive to the value of the **Authorization** HTTP header.

#### Request:

- Supported content types are:

    - `application/json`

#### Response:

- Status code 201
- Headers: []

- Supported content types are:

    - `application/json`

- No response body

## GET /api/inventory


- This endpoint is sensitive to the value of the **Authorization** HTTP header.

#### GET Parameters:

- sort
     - **Values**: *[-]created, [-]updated, [-]checkin, [-]name*
     - **Description**: Optional sort key, use `-` for descending sort.
     - This parameter is a **list**. All GET parameters with the name sort[] will forward their values in a list to the handler.

- expand
     - **Values**: *checkins*
     - **Description**: Optional related records to retrieve.
     - This parameter is a **list**. All GET parameters with the name expand[] will forward their values in a list to the handler.


#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json`

- No response body

## POST /api/inventory


- This endpoint is sensitive to the value of the **Authorization** HTTP header.

#### Request:

- Supported content types are:

    - `application/json`

#### Response:

- Status code 201
- Headers: []

- Supported content types are:

    - `application/json`

- No response body

## DELETE /api/inventory/:itemId

#### Captures:

- *itemId*: The ID of the item.


- This endpoint is sensitive to the value of the **Authorization** HTTP header.

#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json`

- Response body as below.

```javascript
[]
```

## GET /api/inventory/:itemId

#### Captures:

- *itemId*: The ID of the item.


- This endpoint is sensitive to the value of the **Authorization** HTTP header.

#### GET Parameters:

- expand
     - **Values**: *checkins*
     - **Description**: Optional related records to retrieve.
     - This parameter is a **list**. All GET parameters with the name expand[] will forward their values in a list to the handler.


#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json`

- No response body

## PUT /api/inventory/:itemId

#### Captures:

- *itemId*: The ID of the item.


- This endpoint is sensitive to the value of the **Authorization** HTTP header.

#### Request:

- Supported content types are:

    - `application/json`

#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json`

- No response body

## GET /api/inventory/:itemId/history

#### Captures:

- *itemId*: The ID of the item.


- This endpoint is sensitive to the value of the **Authorization** HTTP header.

#### GET Parameters:

- sort
     - **Values**: *[-]date*
     - **Description**: Optional sort key, use `-` for descending sort.
     - This parameter is a **list**. All GET parameters with the name sort[] will forward their values in a list to the handler.

- expand
     - **Values**: *room, user*
     - **Description**: Optional related records to retrieve.
     - This parameter is a **list**. All GET parameters with the name expand[] will forward their values in a list to the handler.


#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json`

- Blah

```javascript
[{"room":3,"user":4,"date":"2015-11-04T11:00:00Z","item":2,"id":99}]
```

- blah blah

```javascript
[{"room":3,"user":4,"date":"2015-11-04T11:00:00Z","item":2,"id":99}]
```

## POST /api/inventory/:itemId/history

#### Captures:

- *itemId*: The ID of the item.


- This endpoint is sensitive to the value of the **Authorization** HTTP header.

#### Request:

- Supported content types are:

    - `application/json`

- Example: `application/json`

```javascript
{"room":3,"user":4,"date":"2015-11-04T11:00:00Z","item":2}
```

#### Response:

- Status code 201
- Headers: []

- Supported content types are:

    - `application/json`

- Response body as below.

```javascript
{"room":3,"user":4,"date":"2015-11-04T11:00:00Z","item":2,"id":99}
```

## GET /api/inventory/:itemId/history/:checkinId

#### Captures:

- *itemId*: The ID of the item.
- *checkinId*: The ID of the check-in.


- This endpoint is sensitive to the value of the **Authorization** HTTP header.

#### GET Parameters:

- expand
     - **Values**: *room, user*
     - **Description**: Optional related records to retrieve.
     - This parameter is a **list**. All GET parameters with the name expand[] will forward their values in a list to the handler.


#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json`

- Response body as below.

```javascript
{"room":3,"user":4,"date":"2015-11-04T11:00:00Z","item":2,"id":99}
```

## GET /api/inventory/:itemId/history/latest

#### Captures:

- *itemId*: The ID of the item.


- This endpoint is sensitive to the value of the **Authorization** HTTP header.

#### GET Parameters:

- expand
     - **Values**: *room, user*
     - **Description**: Optional related records to retrieve.
     - This parameter is a **list**. All GET parameters with the name expand[] will forward their values in a list to the handler.


#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json`

- Response body as below.

```javascript
{"room":3,"user":4,"date":"2015-11-04T11:00:00Z","item":2,"id":99}
```

## GET /api/item-types


- This endpoint is sensitive to the value of the **Authorization** HTTP header.

#### GET Parameters:

- expand
     - **Values**: *properties*
     - **Description**: Optional related records to retrieve.
     - This parameter is a **list**. All GET parameters with the name expand[] will forward their values in a list to the handler.


#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json`

- No response body

## POST /api/item-types


- This endpoint is sensitive to the value of the **Authorization** HTTP header.

#### Request:

- Supported content types are:

    - `application/json`

#### Response:

- Status code 201
- Headers: []

- Supported content types are:

    - `application/json`

- No response body

## DELETE /api/item-types/:typeId

#### Captures:

- *typeId*: The ID of the item type.


- This endpoint is sensitive to the value of the **Authorization** HTTP header.

#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json`

- Response body as below.

```javascript
[]
```

## GET /api/item-types/:typeId

#### Captures:

- *typeId*: The ID of the item type.


- This endpoint is sensitive to the value of the **Authorization** HTTP header.

#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json`

- No response body

## PUT /api/item-types/:typeId

#### Captures:

- *typeId*: The ID of the item type.


- This endpoint is sensitive to the value of the **Authorization** HTTP header.

#### Request:

- Supported content types are:

    - `application/json`

#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json`

- No response body

