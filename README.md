
## Overview

Erlang Modebus Client/Server

## Usage

### Connect

```
{ok, Device} = emodbus:connect("localhost", 502).
```

### Read Coils

```
emodbus:read_coils(Device, Offset, Count).
```

### Read HRegs

```
emodbus:read_hregs(Device, Offset, Count).
```

### Write Coil

```
%% Coil = 0 | 1
emodbus:write_coil(Device, Offset, Coil).
```

### Write HReg

```
emodbus:write_hreg(Device, Offset, Value).
```

### Disconnect

```
emodubs:disconnect(Device).

```

## License

The MIT License

## Author

feng at emqtt.io

