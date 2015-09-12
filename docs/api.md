# Facilities Management

## `/api/buildings` (BuildingListR)

The collection of *Building* resources.

### GET
### POST

## `/api/buildings/#BuildingId` (BuildingR)

An individual *Building* resource.

### GET
### PUT
### DELETE

## `/api/buildings/#BuildingId/rooms` (RoomListR)

A *Building* resource's collection of *Room* resources.

### GET
### POST

## `/api/buildings/#BuildingId/rooms/#RoomId` (RoomR)

An individual *Room* resource.

### GET
### PUT
### DELETE

# Inventory Management

## `/api/inventory` (InventoryR)

The master inventory list, the collection of all *Item* resources.

### GET
### POST

## `/api/buildings/#BuildingId/rooms/#RoomId/inventory` (RoomInventoryR)

A *Room* resource's inventory list, a collection of *Item* resources.

### GET
### PUT

## `/api/inventory/#ItemId` (ItemR)

An individual *Item* resource.

### GET
### PUT
### DELETE

## `/api/inventory/#ItemId/history` (ItemHistoryR)

An *Item* resource's check-in history, a collection of *CheckIn* resources.

### GET
### POST

## `/api/inventory/#ItemId/history/#CheckInId` (ItemCheckInR)

An individual *CheckIn* resource.

### GET

# Reporting 

## `/api/reports/reconciliation` (ReportReconciliationR)

The reconciliation report.

### GET

## `/api/reports/by-type` (ReportByTypeR)

### GET

