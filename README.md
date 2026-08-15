# zcl_ce — Generic Conversion Exit

Utilería estática para convertir **cualquier valor elemental** entre su formato interno y su formato de visualización (y viceversa), sin tener que saber de antemano el tipo del dato. Resuelve en un solo punto:

- **Conversion exits** declarados en el elemento de datos (`ALPHA`, `CUNIT`, etc.), detectados vía RTTS (`edit_mask`).
- **Notación decimal del usuario** (`DCPFM`): separador decimal y de miles según los valores fijos del usuario.
- **Monedas** (`CURR`): desplazamiento de decimales según `TCURX` (p. ej. `CLP`, `JPY` con 0 decimales) y formateo con la cantidad de decimales correcta.
- **Cantidades** (`QUAN`): formateo con `WRITE ... UNIT` según la unidad de medida.
- **Fechas y horas**: `20220330` ↔ `30.03.2022`, `235959` ↔ `23:59:59`.
- **Números** (`int`, `float`, `dec`, `packed`): parseo y formateo respetando la notación del usuario.

Diseñada para capas de entrada/salida genéricas: ALVs editables, carga de archivos, interfaces, mensajes.

## Instalación

Importar con [abapGit](https://abapgit.org). Requiere ABAP 7.40 o superior. Sin dependencias de otros paquetes Z.

## API principal

| Método | Descripción |
|---|---|
| `output( input waers msehi changing output )` | Interno → visualización. El tipo de `INPUT` decide la regla; `WAERS` o `MSEHI` activan lógica de moneda/cantidad |
| `output_string( input waers msehi ) → string` | Igual que `output` pero funcional y sin excepciones (las absorbe); cómodo para concatenaciones |
| `input( input waers msehi changing output )` | Visualización → interno. El tipo de `OUTPUT` (destino) decide la regla |
| `call_ce_input / call_ce_output / call_ce_function` | Llamada directa a un conversion exit por nombre (`CONVERSION_EXIT_<exit>_<INPUT/OUTPUT>`) |
| `get_user_decimal_sep( )` / `get_user_group_sep( )` | Separadores decimal y de miles del usuario actual (cacheados) |
| `get_waers_dec( waers )` | Decimales de la moneda (cacheado; si no está en `TCURX` asume 2) |
| `get_waers_shift_in / get_waers_shift_out` | Exponente de corrección de decimales para importes en moneda |

Solo se admiten tipos **elementales**: estructuras, tablas y referencias levantan la excepción `ZCX_CE_T100` (mensaje `ZCL_CE 000`).

### Reglas por tipo

| Tipo del dato | OUTPUT (interno → display) | INPUT (display → interno) |
|---|---|---|
| Con conversion exit | `CONVERSION_EXIT_*_OUTPUT` | `CONVERSION_EXIT_*_INPUT` |
| `c` / `string` | copia directa | copia directa |
| `n` (NUMC) | exit `ALPHA` OUTPUT | exit `ALPHA` INPUT |
| `i`, `f`, `decfloat` | `WRITE` según usuario | quita separador de miles, normaliza decimal |
| `p` sin moneda/unidad | `WRITE` (formato simple) | idem numérico |
| `p` + `WAERS` | shift de decimales `TCURX` + `WRITE DECIMALS` | shift inverso |
| `p` + `MSEHI` | `WRITE ... UNIT` | idem numérico |
| `d` | `DD.MM.YYYY` | `YYYYMMDD` |
| `t` | `HH:MM:SS` | `HHMMSS` |

## Ejemplos

```abap
data: lv_amount type p length 16 decimals 2 value '1234.00'
    , lv_str    type string
    , lv_back   like lv_amount.

" CLP no tiene decimales → muestra '1.234'
zcl_ce=>output( exporting input = lv_amount waers = 'CLP' changing output = lv_str ).

" y vuelve al formato interno 1234.00
zcl_ce=>input(  exporting input = lv_str    waers = 'CLP' changing output = lv_back ).

" Cualquier valor a string, sin try/catch
data(texto) = zcl_ce=>output_string( input = sy-datum ).   " '15.08.2026'
```

## Excepción `ZCX_CE_T100`

Excepción `CX_STATIC_CHECK` basada en mensajes T100 (`IF_T100_MESSAGE` / `IF_T100_DYN_MSG`), reutilizable en otros desarrollos:

- `zcx_ce_t100=>raise( msgid msgno msgv1..4 )`: levanta la excepción con cualquier mensaje T100; por defecto toma `SY-MSG*` (útil tras un `CALL FUNCTION` con `sy-subrc ne 0`). Las variables se normalizan con `zcl_ce=>output`.
- `get_msgid_msgno( )`: retorna id+número concatenados (p. ej. `ZCL_CE003`) para asserts y manejo por código.

### Clase de mensajes `ZCL_CE`

| Nº | Uso |
|---|---|
| `000` | Tipo no soportado (no elemental) en `input`/`output` |
| `001` | Error inesperado llamando módulo de función `&` |
| `003` | Overflow al convertir un importe en moneda |

> Los textos de los mensajes se mantienen en el sistema (SE91); abapGit solo serializa la cabecera de la clase de mensajes.

## Tests

`ZCL_CE` incluye una suite ABAP Unit (`ltcl_test`) que cubre ida y vuelta (`output` + `input`) para QUAN, CURR, DEC, INT, FLOAT, NUMC, CHAR, STRING, fecha, hora, overflow de moneda y rechazo de tipos no soportados (objetos y estructuras). Ejecutar con `Ctrl+Shift+F10` en ADT o SE80 → Test Unit.
