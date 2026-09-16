# Contribuir a bchR

Gracias por colaborar. El proyecto está en desarrollo y aún necesita contrastar
su contrato con respuestas autenticadas del BCH. No presentes fixtures ficticios
como evidencia de cobertura oficial ni añadas equivalencias de frecuencia por suposición.

## Antes de proponer un cambio

1. Revisa README y las incidencias existentes cuando el repositorio esté disponible.
2. Explica el problema, resultado esperado y versión de R/paquete. No incluyas claves.
3. Para cambios de API, frecuencia, unidades o métodos económicos, acuerda primero
   el contrato y aporta una fuente primaria verificable.

## Flujo de desarrollo

- Crea una rama desde `main` y mantén los cambios enfocados.
- Instala dependencias de desarrollo, abre `bchR.Rproj` y usa `devtools::load_all()`.
- Escribe pruebas offline reproducibles con datos sintéticos o ejemplos públicos
  autorizados y saneados. No grabes cassettes con una clave real.
- Edita documentación en comentarios roxygen de R/, luego ejecuta
  `devtools::document()`. Incluye R/ y man/ actualizados en la misma propuesta.
- Ejecuta `devtools::test()` y `devtools::check(args = "--no-manual")`.
- Añade una nota en NEWS.md si cambia el comportamiento visible.
- Abre un pull request; describe las pruebas realmente ejecutadas y sus límites.

La CI estándar no utiliza BCH_API_KEY. No añadas accesos live en tests, viñetas,
ejemplos evaluados, hooks de instalación ni al cargar el paquete. Las pruebas con
la API real son locales, deliberadas y separadas.

## Datos, seguridad y revisión

Preserva IDs como texto, fechas crudas, frecuencia original y procedencia. No
rellenes huecos ni combines unidades silenciosamente. No incluyas datos oficiales,
marcas o código de terceros sin verificar sus derechos de reutilización.

Informa qué partes de tu contribución recibieron asistencia de IA y cómo fueron
revisadas. Las decisiones científicas y la validación siguen siendo responsabilidad
de quienes contribuyen y revisan. No uses estos materiales para ocultar asistencia
en convocatorias que la prohíban.

Consulta [SECURITY.md](SECURITY.md) para reportes sensibles. El código del paquete
usa la [licencia MIT](LICENSE.md). Las contribuciones de código destinadas a
integrarse deben ser compatibles con esa licencia. Conservas tu autoría y no se
exige una cesión de titularidad. Declara el origen y la licencia de material de
terceros; no se presume una concesión de derechos sobre aportaciones ajenas.
