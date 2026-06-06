import logging
import time
import traceback
import uuid

from fastapi import FastAPI, Request
from fastapi.responses import JSONResponse

LOG_FORMAT = "[%(levelname)s] [%(asctime)s] [%(name)s] %(message)s"


def configure_logging():
    logging.basicConfig(
        level=logging.INFO,
        format=LOG_FORMAT,
        datefmt="%Y-%m-%d %H:%M:%S",
    )


def add_request_logging_middleware(app: FastAPI):
    @app.middleware("http")
    async def log_requests(request: Request, call_next):
        request_id = str(uuid.uuid4())
        request.state.request_id = request_id
        logger = logging.getLogger("api.middleware")
        logger.info(f"Request: {request.method} {request.url.path} [{request_id}]")
        start_time = time.time()

        try:
            response = await call_next(request)
        except Exception as exc:
            logger.error(
                f"Unhandled error [{request_id}]: {exc}\n{traceback.format_exc()}"
            )
            response = JSONResponse(
                status_code=500,
                content={
                    "status": "error",
                    "code": "INTERNAL_ERROR",
                    "message": "An unexpected error occurred",
                },
            )
            response.headers["X-Request-ID"] = request_id
            return response

        process_time = time.time() - start_time
        duration_ms = round(process_time * 1000, 3)
        response.headers["X-Request-ID"] = request_id
        logger.info(
            f"Response: {request.method} {request.url.path} {response.status_code} "
            f"{duration_ms}ms [{request_id}]"
        )
        return response
