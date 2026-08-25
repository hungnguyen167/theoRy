from fastapi import FastAPI, HTTPException
from fastapi.exceptions import RequestValidationError
from fastapi.responses import JSONResponse
from api.routes import router
from api.middleware import configure_logging, add_request_logging_middleware
from api.version import API_VERSION

app = FastAPI(title="Theory Engine", version=API_VERSION)

configure_logging()
add_request_logging_middleware(app)
app.include_router(router, prefix="/api/v1")


@app.exception_handler(RequestValidationError)
async def validation_exception_handler(request, exc):
    return JSONResponse(
        status_code=422,
        content={
            "status": "error",
            "code": "VALIDATION_ERROR",
            "message": str(exc),
        },
    )


@app.exception_handler(HTTPException)
async def http_exception_handler(request, exc):
    content = {"status": "error"}
    if isinstance(exc.detail, dict):
        content.update(exc.detail)
    else:
        content["code"] = "HTTP_ERROR"
        content["message"] = str(exc.detail)
    return JSONResponse(status_code=exc.status_code, content=content)
