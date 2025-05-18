# chatLLM 0.1.2 (Upcoming Release – May 2025)

## New Features

- **DeepSeek Integration**  
  `chat_llm()` now supports **DeepSeek** as a backend provider. This expands the range of available language models and increases flexibility for users selecting different inference engines.

- **Alibaba DashScope Integration**  
  You can now use models from **Alibaba Cloud’s Model Studio (DashScope)** via OpenAI-compatible endpoints. This allows users in mainland China and beyond to easily integrate powerful **Qwen-series** models (like `qwen-plus`, `qwen-turbo`, and others) using the same `chat_llm()` interface.

- **GitHub Copilot-Compatible Model Integration**  
  You can now use models hosted through **GitHub Copilot-compatible endpoints**. This allows seamless integration with custom-hosted or proxy-accessible models, making it easier to experiment with private or specialized deployments.

- **Model Catalog Access**  
  `chat_llm()` now supports listing **all available models across all supported providers**. This makes it easier to discover and compare model options before selecting one for your workflow.
