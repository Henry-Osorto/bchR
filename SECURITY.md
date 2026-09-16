# Seguridad de bchR

## Credenciales

Cada usuario aporta su propia BCH_API_KEY. Nunca la incluyas en código, URLs,
GitHub, issues, pull requests, capturas, registros o archivos HTML. El correo usado
para suscribirse al BCH no convierte la clave en una credencial redistribuible.
El paquete no debe proporcionar una clave pública por defecto.

Los workflows estándar no necesitan una clave BCH, ni un secreto del repositorio
con ese nombre. Un token de GitHub sirve para GitHub, no autentica ante el BCH.

## Informar una vulnerabilidad

No publiques detalles explotables ni secretos en un issue. Cuando el mantenedor
habilite Private vulnerability reporting, utiliza la sección Security del repositorio.
Como alternativa, contacta a henry.osorto@unah.edu.hn describiendo el problema de
forma general y sin enviar la credencial. No se promete un plazo de respuesta.

Incluye versión del paquete, impacto y reproducción mínima con valores ficticios.
Este snapshot es de desarrollo; aún no existe una política de soporte de releases.

## Si una clave se filtra

Revócala o rótala en el servicio emisor primero. Retirarla del archivo actual no
la borra del historial, clones, cachés o registros. Revisa el alcance y coordina una
limpieza de historial siguiendo las instrucciones de GitHub; no hagas force-push
ni elimines registros compartidos sin coordinación. Para una clave BCH, revisa
también la cuenta y las opciones de revocación que ofrezca su portal.
